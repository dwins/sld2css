import scala.collection.JavaConversions._
import org.opengis.filter.Filter
import org.opengis.filter.expression.Expression
import org.geotools.factory.CommonFactoryFinder.getStyleFactory
import org.geotools.styling.{
  ExternalGraphic, Fill, Font, Graphic, LineSymbolizer, Mark, PointSymbolizer,
  PolygonSymbolizer, RasterSymbolizer, SLDParser, Stroke, Symbolizer,
  TextSymbolizer2
}

package object sld2css {
  def sldAsCss(f: java.io.File): String = {
    val style = {
      val parser = new SLDParser(getStyleFactory(null), f)
      parser.readXML()(0)
    }

    val property = (n: String) => (e: Expression) => "%s: %s;" format (n, e)
    val filterSelector = (x: Filter) => x.toString
    val minScaleSelector = (s: Double) => "[@scale > %f]" format s
    val maxScaleSelector = (s: Double) => "[@scale < %f]" format s

    def guard[A, B](f: A => Seq[B]): A => Seq[B] =
      a => if (a == null) Nil else f(a)

    val fillProperties = guard { (f: Fill) =>
      Seq(
        Option(f.getColor()) map property("fill"),
        Option(f.getOpacity()) map property("fill-opacity")
      ).flatten
    }

    val strokeProperties = guard { (s: Stroke) => 
      Seq(
        Option(s.getOpacity()) map property("stroke-opacity"),
        Option(s.getColor()) map property("stroke"),
        Option(s.getDashArray()) map (d => "stroke-dasharray: %s;" format d),
        Option(s.getDashOffset()) map property("stroke-dashoffset"),
        Option(s.getLineCap()) map property("stroke-linecap"),
        Option(s.getWidth()) map property("stroke-width")
      ).flatten
    }

    val fontProperties = guard { (f: Font) =>
      Seq(
        Option(f.getFamily.head) map property("font-family"),
        Option(f.getSize) map property("font-size"),
        Option(f.getStyle) map property("font-style"),
        Option(f.getWeight) map property("font-weight")
      ).flatten
    }

    val graphicProperties: Graphic => Seq[String] = guard { g =>
      (g.graphicalSymbols.map {
        case mark: Mark =>
          Option(mark.getWellKnownName()) map property("mark")
        case external: ExternalGraphic =>
          Option(external.getOnlineResource()) map ("mark: %s;" format _)
      } ++ Seq(
        Option(g.getOpacity) map property("mark-opacity"),
        Option(g.getSize) map property("mark-size"),
        Option(g.getRotation) map property("mark-rotation")
      )).flatten
    }

    val graphicPseudoProperties: Graphic => Seq[(String, Seq[String])] = guard { g =>
      (g.graphicalSymbols.flatMap {
        case mark: Mark =>
          Some((
            "mark",
            fillProperties(mark.getFill) ++ strokeProperties(mark.getStroke)
          ))
        case _ => Nil
      })
    }

    val labelProperties = guard { (sym: TextSymbolizer2) => 
      Seq(
        Option(sym.getLabel()) map property("label")
      ).flatten
    }

    val symbolizerProperties: Symbolizer => Seq[String] = {
      case poly: PolygonSymbolizer =>
        fillProperties(poly.getFill) ++ strokeProperties(poly.getStroke)
      case line: LineSymbolizer =>
        strokeProperties(line.getStroke)
      case point: PointSymbolizer =>
        graphicProperties(point.getGraphic)
      case text: TextSymbolizer2 =>
        fontProperties(text.getFont) ++
        // haloProperties(text.getHalo) ++
        // labelPlacementProperties(text.getLabelPlacement) ++ 
        fillProperties(text.getFill) ++
        labelProperties(text) ++
        graphicProperties(text.getGraphic)
      case raster: RasterSymbolizer =>
        Nil
    }

    val symbolizerPseudoProperties: Symbolizer => Seq[(String, Seq[String])] = {
      case point: PointSymbolizer =>
        graphicPseudoProperties(point.getGraphic)
      case _ => Nil
    }

    val blocks = 
      for {
        ft <- style.featureTypeStyles
        r <- ft.rules
        pred = Option(r.getFilter)
        minScale = Option(r.getMinScaleDenominator).filter(_ > 0)
        maxScale = Option(r.getMaxScaleDenominator).filterNot(_ isInfinity)
        properties = r.symbolizers flatMap symbolizerProperties
        if properties nonEmpty
      } yield {
        val selector = {
          val generated = 
            Seq(
              pred map filterSelector,
              minScale map minScaleSelector,
              maxScale map maxScaleSelector
            ).flatten.mkString(" ")
          if (generated nonEmpty) generated else "*"
        }

        val body = properties.sorted.mkString("{\n    ", "\n    ", "\n}")
        val mainRule = "%s %s" format (selector, body) 
        val pseudoProperties = r.symbolizers flatMap symbolizerPseudoProperties
        val additionalRules =
          for {
            (pseudoclass, properties) <- pseudoProperties
            selector_ = selector + " :" + pseudoclass
            body = properties.sorted.mkString("{\n    ", "\n    ", "\n}")
          } yield "%s %s" format (selector_, body)
        mainRule +: additionalRules
      }

    blocks.flatten mkString "\n\n"
  }
}

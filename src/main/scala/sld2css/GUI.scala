package sld2css

import scala.collection.JavaConversions._
import scala.swing._

object GUI extends SwingApplication {
  def startup(args: Array[String]) {
    locally {
      import javax.swing.UIManager.{ getInstalledLookAndFeels, setLookAndFeel }
      for (nimbus <- getInstalledLookAndFeels.find(_.getName == "Nimbus"))
        setLookAndFeel(nimbus.getClassName)
    }

    val sldEditor = new EditorPane {
      text = "SLD"
      font = new Font("Monospaced", 0, 12)
    }

    val cssEditor = new EditorPane {
      text = "CSS"
      font = new Font("Monospaced", 0, 12)
    }

    lazy val fileForm = new GridBagPanel {
      val fileChooser = new FileChooser {
        fileSelectionMode = FileChooser.SelectionMode.FilesOnly
      }

      val label = new Label("Choose an SLD file (it won't be modified)")
      val button = Button("Choose...") { 
        try { 
          if (fileChooser.showOpenDialog(this) == FileChooser.Result.Approve) {
            val sldFile = fileChooser.selectedFile
            val sld = {
              import java.io._
              val reader = new BufferedReader(new FileReader(sldFile))
              val body = 
                Iterator.continually(reader.read())
                .takeWhile(_ >= 0)
                .map(_.toChar)
                .mkString
              reader.close()
              body
            }
            val css = sldAsCss(sldFile)
            sldEditor.text = sld
            cssEditor.text = css
          }
        } catch {
          case ex =>
            Dialog.showMessage(this, "Loading file failed due to: " + ex)
        }
      }
      layout += (
        label -> (0, 0),
        button -> (1, 0)
      )
    }

    val tabs = new TabbedPane {
      pages += (
        new TabbedPane.Page("SLD", sldEditor),
        new TabbedPane.Page("CSS", cssEditor)
      )
    }

    val frame = new MainFrame {
      title = "SLD → CSS"
      visible = true
      size = new Dimension(300, 500)
      contents = new BoxPanel(Orientation.Vertical) { 
        contents += (fileForm, tabs)
      }
    }
  }
}

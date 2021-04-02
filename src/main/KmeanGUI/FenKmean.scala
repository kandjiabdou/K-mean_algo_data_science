import java.awt._
import java.util._
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.util.Scanner
import java.lang.NumberFormatException
import javax.swing._
import javax.swing.filechooser.FileNameExtensionFilter


@SerialVersionUID(1L)
object FenMeteo {

  def main(args: Array[String]): Unit = { // Modele
    val meteo = new Nothing("meteo.txt")
    // Controleur
    val fenetre = new FenMeteo(meteo)
    // Vue
    val texte = new Nothing
    val graphique = new Nothing
    val table = new Nothing
    // Initiatlisation graphique FenMeteo et modele pour chaque Vue
    fenetre.ajouterPanneau("Liste", texte)
    fenetre.ajouterPanneau("Graphique", graphique)
    fenetre.ajouterPanneau("Tableau", table)
    meteo.enregistrer(texte)
    meteo.enregistrer(graphique)
    meteo.enregistrer(table)
    // Deuxieme fenetre qui contient seulement le modele
    val fenTableau = new Nothing
    val table1 = new Nothing
    fenTableau.ajouterPanneau(table1)
    meteo.enregistrer(table1)
  }
}

@SerialVersionUID(1L)
class FenMeteo(var modele: Nothing) extends JFrame("Météo M2105") with Nothing {
  this.initComposants()
  this.initEcouteurs()
  this.setDefaultCloseOperation(EXIT_ON_CLOSE)
  this.centrer(0.6)
  this.setVisible(true)
  private var btnSauver = null
  private var btnCharger = null
  private var btnJours = null
  private var btnMois = null
  private var cbbAnnees = null
  private var btnAjouter = null
  private var txtTemp = null
  private var txtPrec = null
  private var lblDate = null
  private var tbpVues = null

  private[Composants] class EcouteurBoutons(var zone: Int, var code: Int) extends ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      zone match {
        case FenMeteo.ZONE_JOURS =>
          System.out.println("Bouton JOUR no " + code)
          var lt = String.valueOf(code) // __/__/
          val scan = new Scanner(lblDate.getText)
          scan.useDelimiter("/")
          scan.next
          lt += "/" + scan.next
          scan.close()
          lblDate.setText(lt + "/")


        case FenMeteo.ZONE_MOIS =>
          System.out.println("Bouton MOIS no " + code)
          var lt = ""
          val scan = new Scanner(lblDate.getText)
          scan.useDelimiter("/")
          lt = scan.next
          lt += "/" + String.valueOf(code)
          scan.close()
          lblDate.setText(lt + "/")


        case FenMeteo.ZONE_AUTRE =>
          code match {
            case FenMeteo.CODE_AJOUTER =>
              System.out.println("J'ajoute")
              val scan = new Scanner(lblDate.getText)
              scan.useDelimiter("/")
              try {
                val jr = scan.nextInt
                val ms = scan.nextInt
                scan.close()
                val tmpt = txtTemp.getText.toInt
                val pct = txtPrec.getText.toInt
                modele.ajouter(jr, ms, cbbAnnees.getSelectedItem.asInstanceOf[Int], tmpt, pct)
              } catch {
                case inptMsExcpt: InputMismatchException =>
                  System.out.println("Le jour (ou le mois) n'est pas selectionné")
                  val message = "Le jour (ou le mois) n'est pas selectionné\n" + "Veillez selectionner correctemment la date !"
                  JOptionPane.showMessageDialog(new JFrame, message, "Erreur de Sélection", JOptionPane.ERROR_MESSAGE)
                case nbrFrtExcpt: NumberFormatException =>
                  System.out.println("La température (ou la pécipitation) n'est pas (bien) saisi ")
                  val message = new String("La température (ou la pécipitation) n'est pas (bien) saisi\n" + "Veillez saisir leur valeur correctement")
                  JOptionPane.showMessageDialog(new JFrame, message, "Erreur de Saisi", JOptionPane.ERROR_MESSAGE)
              }

            case FenMeteo.CODE_CHARGER =>
              System.out.println("Je charge")
              val chooser = new JFileChooser
              val filter = new FileNameExtensionFilter("*.txt", "txt")
              chooser.setFileFilter(filter)
              val returnVal = chooser.showOpenDialog(chooser)
              if (returnVal == JFileChooser.APPROVE_OPTION) {
                System.out.println("You chose to load this file: " + chooser.getSelectedFile.getPath)
                modele.charger(chooser.getSelectedFile.getPath)
              }

            case FenMeteo.CODE_SAUVER =>
              System.out.println("Je sauve")
              val chooser1 = new JFileChooser
              val filter1 = new FileNameExtensionFilter("*.txt", "txt")
              chooser1.setFileFilter(filter1)
              val returnVal1 = chooser1.showOpenDialog(chooser1)
              if (returnVal1 == JFileChooser.APPROVE_OPTION) {
                System.out.println("You chose to open this file: " + chooser1.getSelectedFile.getPath)
                modele.sauver(chooser1.getSelectedFile.getPath, modele.versFichier)
              }

            case _ =>

          }
        case _ =>

      }
    }
  }

  private def initComposants(): Unit = {
    val panPrincipal = new JPanel
    panPrincipal.setLayout(new BorderLayout)
    this.add(panPrincipal)
    panPrincipal.add(buildPanelFichiers, BorderLayout.SOUTH)
    panPrincipal.add(buildPanelAjout, BorderLayout.NORTH)
    panPrincipal.add(buildPanelMeteo, BorderLayout.CENTER)
    panPrincipal.add(buildPanelJours, BorderLayout.WEST)
    panPrincipal.add(buildPanelMois, BorderLayout.EAST)
  }

  def initEcouteurs(): Unit = {
    this.btnCharger.addActionListener(new FenMeteo#EcouteurBoutons(FenMeteo.ZONE_AUTRE, FenMeteo.CODE_CHARGER))
    this.btnSauver.addActionListener(new FenMeteo#EcouteurBoutons(FenMeteo.ZONE_AUTRE, FenMeteo.CODE_SAUVER))
    this.btnAjouter.addActionListener(new FenMeteo#EcouteurBoutons(FenMeteo.ZONE_AUTRE, FenMeteo.CODE_AJOUTER))
    for (i <- 0 until 31) {
      this.btnJours(i).addActionListener(new FenMeteo#EcouteurBoutons(FenMeteo.ZONE_JOURS, i + 1))
    }
    for (i <- 0 until 12) {
      this.btnMois(i).addActionListener(new FenMeteo#EcouteurBoutons(FenMeteo.ZONE_MOIS, i + 1))
    }
  }

  def buildPanelFichiers: JPanel = {
    val pan = new JPanel
    btnCharger = new JButton("Charger")
    pan.add(btnCharger)
    btnSauver = new JButton("Sauver")
    pan.add(btnSauver)
    pan.setBorder(BorderFactory.createEtchedBorder)
    pan
  }

  def buildPanelAjout: JPanel = {
    val pAjout = new JPanel
    lblDate = new JLabel("__/__/")
    pAjout.add(lblDate)
    cbbAnnees = new JComboBox[Integer](FenMeteo.ANNEES)
    pAjout.add(cbbAnnees)
    val lbTemp = new JLabel("Température(°C)")
    pAjout.add(lbTemp)
    txtTemp = new JTextField(3)
    pAjout.add(txtTemp)
    val lbPct = new JLabel("Précipitations (mm)")
    pAjout.add(lbPct)
    txtPrec = new JTextField(3)
    pAjout.add(txtPrec)
    btnAjouter = new JButton("Ajouter")
    pAjout.add(btnAjouter)
    pAjout.setBorder(BorderFactory.createEtchedBorder)
    pAjout
  }

  def buildPanelMeteo: JPanel = {
    val pMeteo = new JPanel(new BorderLayout)
    tbpVues = new JTabbedPane
    pMeteo.add(tbpVues, BorderLayout.CENTER)
    pMeteo.setBorder(BorderFactory.createEtchedBorder)
    pMeteo
  }

  def ajouterPanneau(titre: String, c: JComponent): Unit = {
    tbpVues.add(titre, c)
  }

  def buildPanelJours: JPanel = {
    val pJour = new JPanel
    pJour.setLayout(new GridLayout(0, 3))
    btnJours = new Array[JButton](31)
    for (i <- 0 to 30) {
      val bt = new JButton(String.valueOf(i + 1))
      btnJours(i) = bt
    }
    for (bt <- btnJours) {
      pJour.add(bt)
    }
    pJour.setBorder(BorderFactory.createEtchedBorder)
    pJour
  }

  def buildPanelMois: JPanel = {
    val pMois = new JPanel
    pMois.setLayout(new GridLayout(0, 1))

    btnMois = new Array[JButton](12)
    for (i <- 0 to 11) {
      val bt = new JButton(FenMeteo.MOIS(i))
      btnMois(i) = bt
    }
    for (bt <- btnMois) {
      pMois.add(bt)
    }
    pMois.setBorder(BorderFactory.createEtchedBorder)
    pMois
  }

  def centrer(d: Double): Unit = {
    val dim = Toolkit.getDefaultToolkit.getScreenSize
    val largeur = (d * dim.width).toInt
    val longueur = (d * dim.height).toInt
    this.setBounds((dim.width - largeur) / 2, (dim.height - longueur) / 2, largeur, longueur)
  }
}

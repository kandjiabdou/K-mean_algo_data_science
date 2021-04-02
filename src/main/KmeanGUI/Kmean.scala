import java.io.{File, PrintWriter}
import scala.collection.convert.ImplicitConversions.`collection asJava`
import scala.collection.immutable
import scala.io.Source
import scala.util.Random

class Kmean() extends Modele {
  val fichier : String = "C:\\Users\\abduk\\IdeaProjects\\ProjetScala\\src\\main\\scala\\unNom.txt"
  var donnesBrutes : List[List[String]] = List()
  val clusters : List [Cluster] = List()
  val vues : List [Vue] = List()
  charger()

  def charger() : Unit = {
    val l = Source.fromFile(fichier).getLines
    for(ligne <- l){
      val listLinge = ligne.split(",").toList
      donnesBrutes.add(listLinge)
    }
    //----------------- A revoir
    this.donnesBrutes = Random.shuffle(this.donnesBrutes)
    val lesCentres: immutable.Seq[List[String]] = this.donnesBrutes.take(k)
    for(l <- lesCentres) {
      this.clusters = new Cluster(List(), extraire_cordonnes(l, col1, col2)) :: this.clusters
    }
  }

  def enregistrer(v: Vue): Unit = {
    v.setModele(this)
    vues.add(v)
  }

  private def notifierToutesVues(): Unit = {
    println("le nombre de vue maj : " + vues.size)
    for (v <- vues) {
      v.notifierChangement()
    }
  }

  def sauver() : Unit ={
    val pw = new PrintWriter(new File(this.fichier ))
    var string : String = "";

    var integer:Int = 1
    for(cluster <- clusters){
      for(point <- cluster.listePoints){
        string = string + "cluster"+ integer +"\t" + point.x +"\t" + point.y + "\n";
      }
      integer+=1
    }
    integer = 1
    for(cluster<-clusters){
      string = string + "center\t" + cluster.centre.x +"\t" + cluster.centre.y + "\n";
      integer+=1
    }
    pw.write(string)
    pw.close
  }
  def extraire_cordonnes(l : List[String], x : Int = 0, y : Int ) : Point = {
    new Point(l(x).toDouble,l(y).toDouble, l(4))
  }
  override def toString() : String = {
    var str = ""
    for(cluster <- this.clusters){
      str += "-- Le centre du cluster : "+cluster.centre +" la taille  : "+cluster.listePoints.size
    }
    str
  }

}

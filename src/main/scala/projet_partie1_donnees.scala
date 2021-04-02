import scala.io.Source
import scala.util.Random
import smile.plot.swing.plot
import smile.read
import java.awt.Dimension
import java.awt.Toolkit
import java.io.{File, PrintWriter}

//import plotly._, element._, layout._, Plotly._
object projet_partie1_donnees{
    def main(args: Array[String]){
        /*Question ----------------------------------------------------------
            Extraire le fichier correspondant, idéalement stocker les données
            dans une matrice de taille (150 lignes, 5 colonnes, la dernière
            colonne représentant la classe réelle)
        --------------------------------------------------------------------*/

        var ListeDesIris : List[List[String]] = List()
        val l = Source.fromFile("C:\\Users\\abduk\\IdeaProjects\\ProjetScala\\src\\main\\scala\\iris.data").getLines
        for(ligne <- l){
            val listLinge = ligne.split(",").toList
            ListeDesIris = listLinge :: ListeDesIris
        }
        println("la taille de la liste des Iris est "+ListeDesIris.length)
        
        /*Question -------------------------------------------------------------
            Calculer la moyenne, l’écart type, la variance pour
            chaque variable étudié (caractère), commenter vos résultats.
        ----------------------------------------------------------------------*/
        for (caractere <- 0 to 3 ){
            println("la moyenne du caractère est "+(caractere+1)+" : "+moyenneCaratere(ListeDesIris,caractere))
            println("sa variance est : "+varianceCaratere(ListeDesIris,caractere))
            println("et son écart Type est : "+ecartTypeCaratere(ListeDesIris,caractere)+"\n-----------------------------------\n")
        }
        /*
        --> Une variance est toujours positive.
        La valeur d'une variance ne peut être interprétée
        que par comparaison à la valeur d'une norme ou d'une autre variance.
        Si une variance est nulle, cela veut dire que toutes les observations
        sont égales à la moyenne, ce qui implique qu'il n'y a aucune variation de celles-ci.

        --> Plus l'écart type est grand, plus l'erreur type de la moyenne est élevée
        et moins l'estimation de la moyenne de la population est précise.
        */

        /*Question -------------------------------------------------------------
            Calculer le coefficient de corrélation entre toutes les variables 2 à 2,
            projeter et commenter.
        ----------------------------------------------------------------------*/
        println("La covariance de 1 et 2 est : "+coVarianceCaratere(ListeDesIris,0,1))
        println("La covariance de 1 et 3 est : "+coVarianceCaratere(ListeDesIris,0,2))
        println("La covariance de 1 et 4 est : "+coVarianceCaratere(ListeDesIris,0,3))
        println("La covariance de 2 et 3 est : "+coVarianceCaratere(ListeDesIris,1,2))
        println("La covariance de 2 et 4 est : "+coVarianceCaratere(ListeDesIris,1,3))
        println("La covariance de 3 et 4 est : "+coVarianceCaratere(ListeDesIris,2,3))

        println("Le coefficient de corrélation entre 1 et 2 est : "+coefCorrelation(ListeDesIris,0,1))
        println("Le coefficient de corrélation entre 1 et 3 est : "+coefCorrelation(ListeDesIris,0,2))
        println("Le coefficient de corrélation entre 1 et 4 est : "+coefCorrelation(ListeDesIris,0,3))
        println("Le coefficient de corrélation entre 2 et 3 est : "+coefCorrelation(ListeDesIris,1,2))
        println("Le coefficient de corrélation entre 2 et 4 est : "+coefCorrelation(ListeDesIris,1,3))
        println("Le coefficient de corrélation entre 3 et 4 est : "+coefCorrelation(ListeDesIris,2,3))
        

        println("----------------------------------------------------------------------")

        println(" 0 init ----------------------------------------------------------------------")
        val k = 6
        etudierLaCombinaison(ListeDesIris, 0,1, k,"unNom.txt")
        etudierLaCombinaison(ListeDesIris, 0,2, k, "unNom1.txt")
        etudierLaCombinaison(ListeDesIris, 0,3, k,"unNom2.txt")
        etudierLaCombinaison(ListeDesIris, 1,2, k,"unNom3.txt")
        etudierLaCombinaison(ListeDesIris, 1,3, k,"unNom4.txt")
        etudierLaCombinaison(ListeDesIris, 2,3, k,"unNom5.txt")


    }
    def etudierLaCombinaison(ListeDesIris : List[List[String]],c1 : Int, c2 : Int, k : Int,nomF: String) : Unit = {
        val kmean = new Kmean(ListeDesIris, c1, c2, k)

        kmean.initialiser()
        kmean.afficher_les_centres()
        var lastCentres : List[Point] = kmean.clusters.map( _.centre)
        var b = false
        var i = 0
        while(!b && i<20 ){
            println(s"-------------------------------------- $i eme affectation --------------------------------")
            println(" | B = "+b+" on continue .......")
            kmean.affecter()
            kmean.afficher_les_centres()
            b = true
            val lesNcentres : List[Point] = kmean.clusters.map( _.centre)

            for(j <- 0 until lesNcentres.length){
                b = b && lesNcentres(j).equal(lastCentres(j))
            }
            lastCentres = lesNcentres
            i += 1
        }
        println(" | B = "+b+" on arrête  .......")
        sauverDonneesClusters(kmean, nomF)

        var data = read.csv("C:/Users/abduk/IdeaProjects/ProjetScala/src/main/scala/"+nomF, delimiter='\t', header=false)
        //var fentre = plot(data, "V3", "V2", "V1", '#').window()
        val canvas =plot(data, "V3", "V2", "V1", '#')
        val fentre = canvas.window()
        fentre.setTitle("La colonne "+c1.toString + " et "+c2.toString)
        val dim = Toolkit.getDefaultToolkit.getScreenSize
        val largeur = (dim.width/3).toInt
        val longueur = (dim.height/2).toInt
        fentre.setBounds(c1*c2+largeur, c1*c2+longueur, largeur, longueur)
        //println(fentre.getSize)
    }
    
    def moyenneCaratere(matrice : List[List[String]], caractere : Int) : Double = {
        var moyenne : Double = 0
        for(l <- matrice ){
            moyenne += l(caractere).toDouble
        }
        moyenne = moyenne / matrice.length
        moyenne
    }

    def varianceCaratere(matrice : List[List[String]], caractere : Int) : Double = {
        val moyenne : Double = moyenneCaratere(matrice, caractere)
        var variance : Double = 0
        for(l <- matrice ){
            variance += (l(caractere).toDouble - moyenne)*(l(caractere).toDouble - moyenne)
        }
        variance = variance /matrice.length
        variance
    }

    def ecartTypeCaratere(matrice : List[List[String]], caractere : Int) : Double = {
        Math.sqrt(varianceCaratere(matrice,caractere))
    }
    def coVarianceCaratere(matrice : List[List[String]], c1 : Int, c2 : Int) : Double = {
        val m1 : Double = moyenneCaratere(matrice, c1)
        val m2 : Double = moyenneCaratere(matrice, c2)

        var coVariance : Double = 0
        for(l <- matrice ){
            coVariance += (l(c1).toDouble - m1)*(l(c2).toDouble - m2)
        }
        coVariance = coVariance / matrice.length
        coVariance
    }
    def coefCorrelation(matrice : List[List[String]], c1 : Int, c2 : Int) : Double = {
        coVarianceCaratere(matrice,c1,c2)/ecartTypeCaratere(matrice,c1)*ecartTypeCaratere(matrice,c2)
    }
    def sauverDonneesClusters(donnees : Kmean, nomFichier : String) : Unit ={
        val printWriter = new PrintWriter(new File("C:\\Users\\abduk\\IdeaProjects\\ProjetScala\\src\\main\\scala\\"+nomFichier ))
        var string : String = "";

        var integer:Int = 1
        for(cluster <- donnees.clusters){
            for(point <- cluster.listePoints){
                string = string + "cluster"+ integer +"\t" + point.x +"\t" + point.y + "\n";
            }
            integer+=1
        }
        integer = 1
        for(cluster<-donnees.clusters){
            string = string + "center\t" + cluster.centre.x +"\t" + cluster.centre.y + "\n";
            integer+=1
        }
        string += "Moyenne\t" + moyenneCaratere(donnees.ListeD, donnees.col1) +"\t" + moyenneCaratere(donnees.ListeD, donnees.col2) + "\n";
        printWriter.write(string)
        printWriter.close
    }
}


class Cluster(var listePoints : List[Point], var centre : Point = new Point()){
    def centrer() : Unit = {
        centre = new Point(listePoints.map(_.x).sum / listePoints.size, listePoints.map(_.y).sum / listePoints.size)
    }

    def afficher() : Unit = {
        println("-- Le centre du cluster : "+this.centre +" la taille  : "+listePoints.size)
        for(p <- this.listePoints) println(p)
    }

    def afficher_centre() : Unit = {
        println("-- Le centre du cluster : "+this.centre +" la taille  : "+listePoints.size)
    }
    def ajouter(p : Point) : Unit = {
        this.listePoints = p :: this.listePoints
    }
}
class Point(val x : Double = 0, val y : Double = 0, val espece : String = ""){
    def distance(p: Point) : Double = Math.sqrt((this.x - p.x)*(this.x - p.x)+(this.y - p.y)*(this.y - p.y))
    override def toString(): String = {s"P($x.3f, $y.3f)_$espece"}
    def equal(p : Point) : Boolean = {
        this.x == p.x && this.y == p.y
    }
}

class Kmean(var ListeD : List[List[String]], val col1 : Int = 0, val col2 : Int = 1 , val k : Int =2 ){
    var clusters : List [Cluster] = List()

    def initialiser() : Unit = {
        this.ListeD = Random.shuffle(this.ListeD)
        val lesCentres = this.ListeD.take(k)
        for(l <- lesCentres) {
            this.clusters = new Cluster(List(), extraire_cordonnes(l, col1, col2)) :: this.clusters
        }
    }
    def ClusterKmeeanAlgo() : Unit = {
        println("ok")
    }
    def extraire_cordonnes(l : List[String], x : Int = 0, y : Int ) : Point = {

        new Point(l(x).toDouble,l(y).toDouble, l(4))
    }
    def infoClusters() : Unit = {
        for(c <- this.clusters) c.afficher()
    }
    def afficher_les_centres() : Unit = {
        for(c <- this.clusters) c.afficher_centre()
        //for(c <- this.clusters) c.afficher()

    }
    def affecter() : Unit = {
        for( c <- this.clusters){
            c.listePoints = List()
        }
        for (l <- this.ListeD){
            val pointl = extraire_cordonnes(l, this.col1, this.col2)
            var ind = 0
            var dist : Double = 10000000
            for( i <- 0 until this.k){
                val dist2 : Double = this.clusters(i).centre.distance(pointl)
                if ( dist2 < dist || dist2.isNaN ){
                    ind = i
                    dist = dist2
                    //if ( dist2.isNaN ) println("le point "+pointl+" et le centre "+this.clusters(i).centre +" sont trop proches")
                }
            }
            
            this.clusters(ind).ajouter(pointl)
        }
        for( c <- this.clusters){
            c.centrer()
        }
    }
    
}
/*
println("----------------------------------------------------------------------")
val p1 = new Point(1,9)
val p2 = new Point(7,2)
val p3 = new Point(8,4)
val p4 = new Point(6,8)
val p5 = new Point(1,1)
val lp = List(p1, p2, p3, p4, p5)
val clt = new Cluster(lp)
clt.afficher()
 */

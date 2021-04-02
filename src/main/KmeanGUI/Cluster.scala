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
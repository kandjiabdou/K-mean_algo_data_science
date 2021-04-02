class Point(val x : Double = 0, val y : Double = 0, val espece : String = ""){
  def distance(p: Point) : Double = Math.sqrt((this.x - p.x)*(this.x - p.x)+(this.y - p.y)*(this.y - p.y))
  override def toString(): String = {s"P($x, $y)_$espece"}
  def equal(p : Point) : Boolean = {
    this.x == p.x && this.y == p.y
  }
}
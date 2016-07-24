println("abc")

def findVectorIndex(c:Char, vector: Vector[Vector[Char]]) = {
  val findY = vector.map(_.indexOf(c))
  val x = findY.indexWhere(_ >= 0)
  (x, findY(x))
}

val vector = Vector(Vector('a', 'b'), Vector('c', 'd'))
findVectorIndex('d', vector)
findVectorIndex('a', vector)
findVectorIndex('b', vector)

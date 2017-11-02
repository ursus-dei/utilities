class Word(val x: Int, val y: Int, val dx: Int, val dy: Int, val text: String) {

  def hasLegalPlacement(max_x: Int, max_y: Int): Boolean = {
    if (dx == 0 && dy == 0) {
      return false
    }
    for (i <- 0 to this.text.length) {
      val x1 = this.x + this.dx * i;
      val y1 = this.y + this.dy * i;
      if (x1 < 0 || y1 < 0 || x1 >= max_x || y1 >= max_y) {
        return false
      }
    }
    true
  }

  def conflictsWith(other: Word): Boolean = {
    for (i <- this.text.indices) {
      val x1 = this.x + this.dx * i
      val y1 = this.y + this.dy * i
      for (j <- other.text.indices) {
        val x2 = other.x + other.dx * j
        val y2 = other.y + other.dy * j
        if (x1 == x2 && y1 == y2) {
          if (this.dx == other.dx && this.dy == other.dy) {
            return true
          }
          if (this.text.substring(i, i + 1) != other.text.substring(j, j + 1)) {
            return true
          }
        }
      }
    }
    return false
  }

  def intersects(other: Word): Boolean = {
    for (i <- this.text.indices) {
      val x1 = this.x + this.dx * i
      val y1 = this.y + this.dy * i
      for (j <- other.text.indices) {
        val x2 = other.x + other.dx * j
        val y2 = other.y + other.dy * j
        if (x1 == x2 && y1 == y2) {
          return true
        }
      }
    }
    false
  }

  override def toString(): String = {
    x + " " + y + " " + dx + " " + dy + " " + text
  }

}

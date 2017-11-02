import scala.collection.mutable.HashSet
import scala.io.Source
import scala.util.Random

object Grid extends App {

  val MAX_X = 25
  val MAX_Y = 25
  val random = new Random

  val string = Source.fromFile("/Users/dbenefi/source.txt").getLines.toList.toString
  val texts = string.toUpperCase().split("[^A-Z][^A-Z]*").toSet.toList.sorted

  var best = List[Word]()

  for (p <- 0 to 100) {
    println(p)
    var words = List[Word]()
    for (i <- texts.indices) {
      if (texts(i).length > 2 && texts(i).length <= 10 && texts(i) != "LIST") {
        val x = random.nextInt(MAX_X)
        val y = random.nextInt(MAX_Y)
        val dx = random.nextInt(3) - 1
        val dy = random.nextInt(3) - 1
        var n = 0
        var word: Word = new Word(x, y, dx, dy, texts(i))
        while ((conflicts(words, word) || !word.hasLegalPlacement(MAX_X, MAX_Y)) && n < 10000) {
          val x = random.nextInt(MAX_X)
          val y = random.nextInt(MAX_Y)
          val dx = random.nextInt(3) - 1
          val dy = random.nextInt(3) - 1
          n += 1
          word = new Word(x, y, dx, dy, texts(i))
        }
        if (!conflicts(words, word) && word.hasLegalPlacement(MAX_X, MAX_Y)) {
          words = words :+ word
        }
      }
      if (score(words) > score(best)) {
        best = words
      }
    }
    println(best.length + "\t" + score(best))
    println()
  }

  var grid = Array.ofDim[Char](MAX_X, MAX_Y)

  var letters = List[Char]();
  for (i <- best.indices) {
    for (j <- 0 until best(i).text.length()) {
      letters = letters :+ best(i).text.charAt(j);
      val x = best(i).x + (j * best(i).dx)
      val y = best(i).y + (j * best(i).dy)
      grid(x)(y) = best(i).text.charAt(j)
    }
  }
  val found = dupCheck(best, grid)
  var flag = true

  while (flag || found != dupCheck(best, grid)) {
    flag = false
    for (x <- 0 until MAX_X) {
      for (y <- 0 until MAX_Y) {
        grid(x)(y) = 0.toChar
      }
    }
    for (i <- best.indices) {
      for (j <- 0 until best(i).text.length()) {
        val x = best(i).x + (j * best(i).dx)
        val y = best(i).y + (j * best(i).dy)
        grid(x)(y) = best(i).text.charAt(j)
      }
    }
    for (x <- 0 until MAX_X) {
      for (y <- 0 until MAX_Y) {
        if (grid(x)(y).asInstanceOf[Int] == 0) {
          val c = random.nextInt(letters.length)
          // + 32
          grid(x)(y) = (letters(c).asInstanceOf[Int]).asInstanceOf[Char]
        }
      }
    }
  }

  for (yy <- 0 until MAX_Y) {
    for (xx <- 0 until MAX_X) {
      print(grid(xx)(yy) + " ")
    }
    println()
  }

  for (i <- best.indices) {
    if (i % 5 == 0) {
      println()
    }
    print("%-12s".format(best(i).text))
  }
  println()

  def conflicts(words: List[Word], word: Word): Boolean = {
    for (i <- words.indices) {
      if (word.conflictsWith(words(i))) {
        return true
      }
    }
    false
  }

  def score(words: List[Word]): Int = {
    var cross = 0
    var letters = 0
    for (i <- 0 until words.length) {
      letters += words(i).text.length
      for (j <- i + 1 until words.length) {
        if (words(i).intersects(words(j))) {
          cross += 10
        }
      }
    }
    return 100 * words.length + cross + letters
  }

  def dupCheck(words: List[Word], grid: Array[Array[Char]]): Int = {
    var n = 0;
    for (i <- words.indices) {
      for (x <- 0 until MAX_X) {
        for (y <- 0 until MAX_Y) {
          for (dx <- -1 to 1) {
            for (dy <- -1 to 1) {
              var m = 1
              for (j <- 0 until words(i).text.length) {
                val x1 = x + j * dx
                val y1 = y + j * dy
                if (x1 >= 0 && x1 < MAX_X && y1 >= 0 && y1 < MAX_Y) {
                  if (grid(x1)(y1) != words(i).text.charAt(j)) {
                    m = 0
                  }
                } else {
                  m = 0
                }

              }
              n += m
            }
          }
        }
      }
    }
    return n;
  }

}
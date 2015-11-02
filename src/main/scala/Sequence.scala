import breeze.linalg.DenseMatrix

/**
 * Created by yosuke on 10/30/15.
 * Sequence class is used to store genomic sequence
 * It also provides k-mer frequency vector by naively counting them
 */

class Sequence(s : String) {

  val seq = s.toCharArray.map(c2b)
  val length = seq.length

  def to_str() = {
    println(seq.map(b2c).mkString)
  }

  def kmerCount(k : Int) = {
    val c = DenseMatrix.zeros[Double](1 << (2 * k), 1)
    for(i <- 0 until (length - k + 1)){
      var kmer = 0
      for(b <- seq.slice(i, i + k)){
        kmer = kmer << 2
        kmer += b
      }
      c(kmer, 0) += 1
    }
    c
  }

  def c2b(c : Char) : Byte = {
    c match {
      case 'A' => 0
      case 'C' => 1
      case 'G' => 2
      case 'T' => 3
      case 'a' => 0
      case 'c' => 1
      case 'g' => 2
      case 't' => 3
      case _ => throw new SeqLetterException(c)
    }
  }

  def b2c(n : Byte) : Char = {
    n match {
      case 0 => 'A'
      case 1 => 'C'
      case 2 => 'G'
      case 3 => 'T'
      case _ => throw new SeqNumberException(n)
    }
  }
}

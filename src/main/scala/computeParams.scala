import breeze.linalg._

/**
 * Created by yosuke on 11/3/15.
 *
 * class to compute parameters q and P
 */
class computeParams(hiC: ReadHiC,
                    featureVector: Array[Option[DenseMatrix[Double]]],
                    k : Int,
                    binSize : Int,
                    minInterval : Int = 0,
                    maxInterval : Int = Integer.MAX_VALUE) {

  private val f_split : String => Array[String] =
    l => l split "\t"

  private val f_toNumeric : Array[String] => Option[(Int, Int, Double)] =
    a => Some((a(0).toInt / hiC.resolution , a(1).toInt / hiC.resolution, a(2).toDouble))
  /**
   * Since Function3 trait does not have 'compose' method,
   * there is a need to use Option[(Int, Int, Double)] rather than (Int, Int, Double)
   */

  private val f_normalize: Option[(Int, Int, Double)] => Option[(Int, Int, Double)] = {
    case Some((i, j, m)) => {
      if (m.isNaN ||
        Math.abs(i - j) > (maxInterval / hiC.resolution) ||
        Math.abs(i - j) < (minInterval / hiC.resolution) ||
        featureVector(i).isEmpty ||
        featureVector(j).isEmpty ||
        (hiC.norm.isDefined && (hiC.norm.get(i).isEmpty || hiC.norm.get(j).isEmpty)) ||
        (hiC.expected.isDefined && hiC.expected.get(Math.abs(i - j)).isEmpty))
        None
      else if (hiC.norm.isDefined && hiC.expected.isDefined)
        Some((i, j, m / (hiC.norm.get(i).get * hiC.norm.get(j).get * hiC.expected.get(Math.abs(i - j)).get)))
      else if (hiC.norm.isDefined && hiC.expected.isEmpty)
        Some((i, j, m / (hiC.norm.get(i).get * hiC.norm.get(j).get)))
      else if (hiC.norm.isEmpty && hiC.expected.isDefined)
        Some((i, j, m / hiC.expected.get(Math.abs(i - j)).get))
      else
        Some((i, j, m))
    }
    case None => None
  }
  /**
   * Function for Normalization and Observed / Expected [O/E] conversion.
   * Here, the existence of corresponding feature vector is also checked
   */

  private val f_compute_dij: Option[(Int, Int, Double)] => Option[(DenseMatrix[Double], Double)] = {
    case Some((i: Int, j: Int, m: Double)) =>
      if(m.isNaN)
        None
      else
        Some(((featureVector(i).get * featureVector(j).get.t).reshape(1 << (4 * k), 1), m))
    case None => None
  }
  /**
   * Compute k-mer pair count vector of length 16^k
   */

  private val f_dij : String => Option[(DenseMatrix[Double], Double)] =
    f_compute_dij compose f_normalize compose f_toNumeric compose f_split
  /**
   * composed function to compute d_{ij} from each line of Hi-C data
   */


  val q = DenseMatrix.zeros[Double](1 << (4 * k), 1)
  val P = DenseMatrix.zeros[Double](1 << (4 * k), 1 << (4 * k))

  hiC.rawData.foreach{ l:String =>
    val dm : Option[(DenseMatrix[Double], Double)] = f_dij(l)
    dm match {
      case Some((dij, mij)) => {
        q += dij * mij
        P += (dij * dij.t) * (1.0 / ((binSize + k - 1) * (binSize + k - 1)))
      }
      case None => Unit
    }
  }


}

import scala.io.Source
/**
 * Created by yosuke on 11/1/15.
 * read HiC data from File
 */
class ReadHiC(datasetPath : String,
              chr : String,
              res : Int,
              normMethod : Option[String] = None,
              expectedMethod : Option[String] = None,
              minInterval : Int = 0,
              maxInterval : Int = Integer.MAX_VALUE) {

  private val path = Array(datasetPath, res2resstr(res) + "_resolution_intrachromosomal", chr, "MAPQGE30").mkString("/")

  /* read normalize vector if necessary */
  val norm:Option[Array[Option[Double]]] = normMethod match {
    case Some(n) => Some(readNorm(n))
    case None => None
  }

  val expected:Option[Array[Option[Double]]] = expectedMethod match {
    case Some(e) => Some(readExpected(e))
    case None => None
  }

  /* read raw contact frequency matrix */
  val data = readObserved(norm, expected, minInterval, maxInterval, res)

  println(s"Hi-C Data is loaded from $path")


  def this(fpath : String, chr : Int, res : Int) = {
    this(fpath, s"chr$chr", res)
  }
/*
  def data(normMethod : Option[String] = None,
           expectedMethod : Option[String] = None,
           minInterval : Int = 0,
           //maxInterval : Int = Integer.MAX_VALUE) = {
           maxInterval : Int = Integer.MAX_VALUE) : scala.collection.parallel.mutable.ParArray[Option[(Int, Int, Double)]] = {

    val normv = getNormVector(normMethod)
    val expectedv = getExpectedVector(expectedMethod)

    val matrix = RAWobserved.map {
      case (i, j, m) if m.isNaN => None
      case (i, j, m) if Math.abs(i - j) > maxInterval => None
      case (i, j, m) if Math.abs(i - j) < minInterval => None
      case (i : Int, j: Int, m: Double) =>
        normv match {
          case Some(norm) =>
            expectedv match {
              case Some(expected) => Some((i, j, m / (norm(i / res) * norm(j / res) * expected(Math.abs(i -j) / res))))
              case None           => Some((i, j, m / (norm(i / res) * norm(j / res))))
            }
          case None =>
            expectedv match {
              case Some(expected) => Some((i, j, m / expected(Math.abs(i -j) / res)))
              case None           => Some((i, j, m))
            }
        }
      case _ => throw new Exception("Wrong input in RAW observed file")
    }
    println(s"$normMethod normalization and $expectedMethod O/E conversion finished")
    matrix
  }

  private def getNormVector(method :Option[String]) : Option[Array[Double]] = {
    method match {
      case Some(m) =>
        m match {
          case "KR"     => Some(KRnorm)
          case "VC"     => Some(VCnorm)
          case "SQRTVC" => Some(SQRTVCnorm)
          case _ => throw new NormMethodException(m)
        }
      case None    => None
    }
  }

  private def getExpectedVector(method :Option[String]) : Option[Array[Double]] = {
    method match {
      case Some(e) =>
        e match {
          case "RAW"    => Some(RAWexpected)
          case "KR"     => Some(KRexpected)
          case "VC"     => Some(VCexpected)
          case "SQRTVC" => Some(SQRTVCexpected)
          case _ => throw new ExpectedMethodException(e)
        }
      case None    => None
    }
  }
**/

  private def readObserved(normVector:Option[Array[Option[Double]]],
                           expectedVector:Option[Array[Option[Double]]],
                           minInterval:Int, maxInterval:Int, res:Int) = {
    val file = Source.fromFile(Array(path, "/", chr, "_", res2resstr(res), ".RAWobserved").mkString("")).getLines().toArray.par

    val f_split = (l : String) => l split "\t"
    val f_parse = (a : Array[String]) => {
      if(a(2).toDouble.isNaN ||
        Math.abs(a(0).toInt - a(1).toInt) > maxInterval ||
        Math.abs(a(0).toInt - a(1).toInt) < minInterval)
        None
      else
        Some((a(0).toInt, a(1).toInt, a(2).toDouble))
    }

    val filtered = file.map (f_parse compose f_split)

    filtered
  }

  private def read(method : String, dataType : String) = {
    /* read normalization vector / expected value */
    val file = Source.fromFile(Array(path, "/", chr, "_", res2resstr(res), ".", method, dataType).mkString("")).getLines().toArray
    val f_convert = (l: String) => if(l.toDouble.isNaN) None else Some(l.toDouble)
    val vector = file.map{f_convert}
    vector
  }

  private def readNorm(method : String) = read(method, "norm")

  private def readExpected(method : String) = read(method, "expected")

  private def res2resstr(res : Int) : String = {
    res match {
      case 1000    => "1kb"
      case 5000    => "5kb"
      case 10000   => "10kb"
      case 25000   => "25kb"
      case 50000   => "50kb"
      case 100000  => "100kb"
      case 250000  => "250kb"
      case 500000  => "500kb"
      case 1000000 => "1Mb"
      case _ => throw new ResSizeException(res)
    }
  }

}

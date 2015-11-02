import scala.io.Source
import java.io.{File, FileReader, BufferedReader}

/**
 * Created by yosuke on 11/1/15.
 * read HiC data from File
 */
class ReadHiC(datasetPath : String,
              chr : String,
              res : Int,
              normMethod : Option[String] = None,
              expectedMethod : Option[String] = None){
  def this(fpath : String, chr : Int, res : Int) = {
    this(fpath, s"chr$chr", res)
  }

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
  val rawData = readRawData()

  val length = rawData.length
  val resolution = res



  //val data = prepObserved(norm, expected, minInterval, maxInterval, res)


/*
  private def prepObserved(normVector:Option[Array[Option[Double]]],
                           expectedVector:Option[Array[Option[Double]]],
                           minInterval:Int, maxInterval:Int, res:Int) = {


    private def getFileName(path: String, chr:String, res:Int): Unit ={
      Array(path, "/", chr, "_", res2resstr(res), ".RAWobserved").mkString("")
    }
    val fileName = getFileName(path, chr, res)


    val reader = new BufferedReader( new FileReader( new File (fileName)))
    var line:String = null

    val buf = scala.collection.mutable.ArrayBuffer.empty[String]

    while( { line = reader.readLine; line != null}){
      buf.append(line)
    }
    reader.close()
    val file = buf.toArray.par

    println(s"Hi-C: extract Hi-C contact matrix by size: \tmin = $minInterval, \tmax = $maxInterval")

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

    println(s"Hi-C: normalization and O/E conversion: " +
            "\tnorm = " + normMethod.getOrElse("None") +
            ", \texpected = " + expectedMethod.getOrElse("None"))

    val data:scala.collection.parallel.mutable.ParArray[Option[(Int, Int, Double)]] = normVector match {
      case Some(norm) =>
        expectedVector match{
          case Some(expected) =>
            filtered.map {
              case Some((i: Int, j: Int, mij: Double)) =>
                if(norm(i / res).isDefined && norm(j / res).isDefined && expected(Math.abs(i - j) / res).isDefined)
                  Some((i, j, mij / (norm(i / res).get * norm(j / res).get * expected(Math.abs(i - j) / res).get)))
                else
                  None
              case None => None
            }
          case None =>
            filtered.map {
              case Some((i: Int, j: Int, mij: Double)) =>
                if(norm(i / res).isDefined && norm(j / res).isDefined)
                  Some((i, j, mij / (norm(i / res).get * norm(j / res).get)))
                else
                  None
              case None => None
            }
        }
      case None =>
        expectedVector match{
          case Some(expected) =>
            filtered.map {
              case Some((i: Int, j: Int, mij: Double)) =>
                if(expected(Math.abs(i - j) / res).isDefined)
                  Some((i, j, mij / expected(Math.abs(i - j) / res).get))
                else
                  None
              case None => None
            }
          case None => filtered
        }
    }

    data
  }
*/
  private def read(method : String, dataType : String) = {
    /* read normalization vector / expected value */
    val file = Source.fromFile(Array(path, "/", chr, "_", res2resstr(res), ".", method, dataType).mkString("")).getLines().toArray
    val f_convert = (l: String) => if(l.toDouble.isNaN) None else Some(l.toDouble)
    val vector = file.map{f_convert}
    vector
  }

  private def readNorm(method : String) = {
    val norm = read(method, "norm")
    println(s"Hi-C Normalize vector [$method] is loaded")
    norm
  }

  private def readExpected(method : String) = {
    val expected = read(method, "expected")
    println(s"Hi-C expected value vector [$method] is loaded")
    expected
  }

  /* read RAWobserved data from file */
  private def readRawData() = {
    val rawDataFileName = Array(path, "/", chr, "_", res2resstr(res), ".RAWobserved").mkString("")
    val reader = new BufferedReader( new FileReader( new File (rawDataFileName)))
    var line:String = null
    val buf = scala.collection.mutable.ArrayBuffer.empty[String]
    while( { line = reader.readLine; line != null}){
      buf.append(line)
    }
    reader.close()
    val rawData = buf.toArray.par
    rawData
  }

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

/**
 * Created by yosuke on 11/1/15.
 * Exception class.
 * bin size
 */

class ResSizeException(res: Int) extends Exception(s"Resolution size $res is not in the dataset")

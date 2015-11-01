/**
 * Created by yosuke on 11/1/15.
 * Exception : unknown normalization method
 */
class NormMethodException(norm : String) extends Exception(s"$norm is not supported method of normalization")

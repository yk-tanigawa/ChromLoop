/**
 * Created by yosuke on 10/30/15.
 * Exception class
 */
class SeqNumberException(n : Byte) extends Exception(s"$n is not a 2-bit Integer")

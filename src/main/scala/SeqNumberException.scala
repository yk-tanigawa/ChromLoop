/**
 * Created by yosuke on 10/30/15.
 * Exception class
 */
class SeqNumberException(n : Byte) extends Exception(n + " is not 2-bit Integer")

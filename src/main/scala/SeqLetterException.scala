/**
 * Created by yosuke on 10/30/15.
 * Exception class
 */
class SeqLetterException(let: Char) extends Exception(s"$let is not in Alphabet")

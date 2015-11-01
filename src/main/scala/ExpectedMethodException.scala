/**
 * Created by yosuke on 11/1/15.
 * Exception class expected method is unknown
 */
class ExpectedMethodException (expected : String) extends Exception (expected + " is not supported method for O/E conversion")

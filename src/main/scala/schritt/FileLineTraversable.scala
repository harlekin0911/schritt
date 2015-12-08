package schritt

import java.io.BufferedReader
import java.io.FileReader
import java.io.File


class FileLineTraversable(file: File) extends Traversable[String] {
    override def foreach[U](f: String => U) : Unit = {
        val input = new BufferedReader(new FileReader(file))
        try {
            var line = input.readLine
            while(line != null) {
                f(line)
                line = input.readLine
            }
        } finally {
            input.close()
        }
    }
    override def toString = "{Lines of " + file.getAbsolutePath + "}"
}

object FileLineTraversable {
    
    def main( args: Array[String]) : Unit = {
        val x = new FileLineTraversable(new java.io.File("test.txt"))
        for { line <- x
              word <- line.split("\\s+")
       } yield Console.println( word)
       
    }
}
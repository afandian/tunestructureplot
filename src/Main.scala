/**
 * Created with IntelliJ IDEA.
 * User: joe
 * Date: 16/02/2013
 * Time: 15:52
 * To change this template use File | Settings | File Templates.
 */

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.sound.midi.Track
import melodysequence.{MelodyMatch, Midi, Plotter}

object Main {

  def plotMidiFile(inputPath : String, outputPath : String) {
    var track = Midi.getTrackFromMidiFile(inputPath, 0)

    track match {
      case Some(track: Track) => {
        val structure = Midi.structureFromTrack(track)
        val tune = structure.asMonophonic()
        val identities = melodysequence.Functions.tuneIdentities(tune)//.slice(0, 20)
        var allPrefixes = melodysequence.Functions.allPrefixesWithSkips(identities.toList, 5)

        // Kludge. Remove short ones.
        allPrefixes = allPrefixes.filter{case MelodyMatch(a, b, length) => length > 3}
        val brackets = allPrefixes.map {case MelodyMatch(a, b, length) => (a, a + length - 1 , b, b + length - 1)}
        val image : BufferedImage = Plotter.plotStructure(tune, brackets)
        ImageIO.write(image, "PNG", new File(outputPath))
      }
      case None => {}
    }
  }

  // Test runner.
  def main(args : Array[String]) {
    new File("testoutput").mkdirs()

    plotMidiFile("testinput/butterfly.mid", "testoutput/butterfly.png")
    plotMidiFile("testinput/ellen-o-grady.mid", "testoutput/ellen-o-grady.png")
    plotMidiFile("testinput/old-favourite.mid", "testoutput/old-favourite.png")
  }
}

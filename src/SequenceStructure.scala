import java.awt.image.BufferedImage

/**
 * User: joe
 * Date: 16/02/2013
 * Time: 14:07
 */

package melodysequence {

import javax.sound.midi._
import java.io.File
import scala._
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map}
import java.awt.image.BufferedImage
import java.awt._
import geom.{Arc2D, Point2D, CubicCurve2D, Path2D}
import scala.Some
import scala.Tuple3
import scala.List

// TODO tail call!

case class Note(offset: Long, duration:  Long, pitch: Int)

// A melody structure.
class MelodyStructure(events: List[Tuple3[Symbol, Long, Int]]) {
  // Convert the list of note on and off durations into a monophonic sequence
  // of (onset, duration, pitch). In the case of polyphony, last one wins!
  def asMonophonic(): Seq[Note] = {
    // Some things are best looped!
    // TODO maybe this would be best recursive.
    var lastOnPitch: Int = -1
    var lastOnset: Long = 0
    var events = ListBuffer[Note]()

    for (event <- this.events) {
      event match {
        case ('NoteOn, time, pitch) => {
          var duration = if (lastOnset == -1) 0; else time - lastOnset

          if (lastOnPitch != -1) {
            // Argh! We've had two note-ons!
            // Append the old one first.
            events.append(Note(time, duration, lastOnPitch))
          }

          lastOnPitch = pitch
          lastOnset = time
        };
        case ('NoteOff, time, pitch) => {

          var duration = if (lastOnset == -1) 0; else time - lastOnset

          if (lastOnPitch == pitch) {
            events.append(Note(lastOnset, duration, pitch))
            lastOnPitch = -1
            lastOnset = -1
          } else {
            // If we're note-offing a pitch that we didn't note-on, just ignore
            // as the other clause will have taken care of it.
            lastOnPitch = -1
            lastOnset = -1
          }
        }
      }
    }

    return events
  }
}

  object Functions {
    // Sequence of tune note identities.
    // In future we can change this to include / exclude duration.
    def tuneIdentities(inp: Seq[Note]) =
      inp.map { case Note(_, duration, pitch) => (pitch, duration)}

    // From a tune structure generate a sequence of brackets that indicate where a sequence of notes
    // is duplicated in form (first range start, end, second range start, end).
    def modalDuration(inp: Seq[Note]) : Double = {
      val lengths = Map[Double, Int]()
      for (note <- inp) {
        lengths.put(note.duration, (lengths.getOrElse(note.duration, 0) + 1))
      }

      (lengths.toList.sortBy {entry => entry._2}).last._1
    }

    // Longest prefix of two ranges in the source string starting at aI and bI
    // Return sequence length (incl zero).
    def longestPrefix(sequence : List[Any], aI : Int, bI : Int, length : Int = 0) : Int =
      (sequence.slice(aI, bI)
        .zip(sequence.slice(bI, sequence.length))
        .takeWhile {case (x: Any, y: Any) => x == y})
        .length

    // For a given starting point (and minimum sequence length) find all the prefixes.
    // Return a stream of (firstIndex, secondIndex, length)
    def prefixForSearchIndex(sequence: List[Any], aI : Int, minOffset : Int) : Seq[(Int, Int, Int)] = {
      (for (bI <- Range(aI + minOffset, sequence.length))
      yield (aI, bI, longestPrefix(sequence, aI, bI))).filter{
        case(_, _, length) => length > 0 && length >= 1
      }
    }

    // All prefixes!
    def allPrefixesSimple(sequence: List[Any], minLength : Int) =
      (for (i <- Range(0, sequence.length))
      yield prefixForSearchIndex(sequence, i, minLength)).flatten

    // All prefixes, skipping comb-type short ones.
    // To tidy up.
    def allPrefixesWithSkips(sequence: List[Any], minLength : Int, i : Int = 0) : Seq[(Int, Int, Int)] = {
      var i = 0
      val prefixes = new mutable.ListBuffer[(Int, Int, Int)]()
      do {
        val elems = prefixForSearchIndex(sequence, i, minLength)

        if (elems.length > 0) {
          prefixes ++= elems

          i += elems.map{case (i : Int, j: Int, length: Int) => length}.max
        }
        else {
          i += 1
        }
      } while (i < sequence.length)

      prefixes
    }
  }

//
object Plotter {
  // Return the modal note duration.
  // This is designed for tunes with lots of identical note lengths.

  // Plot a structure with highlight brackets as a sequence of (from start, from end, to start, to end)
  def plotStructure(notes : Seq[Note], brackets : Seq[(Int, Int, Int, Int)]) : BufferedImage  = {
    // Desired width of modal note length.
    val desiredModalWidth = 10;

    // Height of note.
    var noteHeight = 4

    val modalLength = Functions.modalDuration(notes)
    val horizontalMultiplier = desiredModalWidth / modalLength;
    val minPitch = notes.minBy(note => note.pitch).pitch
    val maxPitch = notes.maxBy(note => note.pitch).pitch
    val pitchRange =  maxPitch - minPitch

    val height = (pitchRange * noteHeight).toInt;
    val width = (notes.map {
      case(Note(offset, duration, pitch)) => (duration * horizontalMultiplier)
    } sum).toInt

    // todo measure max radius and resize to that
    val topMargin = width / 2 // top margin square to accommodate a big arc
    val bottomMargin = 10
    val leftMargin = 10
    val rightMargin = 10

    val canvasWidth = width + leftMargin + rightMargin
    val canvasHeight = height + topMargin + bottomMargin

    val buffer  : BufferedImage = new BufferedImage(canvasWidth, canvasHeight, BufferedImage.TYPE_INT_ARGB)
    val graphics : Graphics2D  = buffer.createGraphics()

    val hints = new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    graphics.setRenderingHints(hints)

    // White background.
    graphics.setPaint(Color.white)
    graphics.fillRect(0, 0, canvasWidth, canvasHeight)

    // Draw margins
    // graphics.setPaint(Color.green)
    // graphics.drawRect(1,1,canvasWidth-2,canvasHeight-2)
    // graphics.drawRect(0+topMargin,0+leftMargin,canvasWidth-leftMargin-rightMargin,canvasHeight-topMargin-bottomMargin)

    val pitchX = (pitch : Int) => leftMargin + (pitch.toFloat * horizontalMultiplier).toInt
    val pitchY = (pitch: Int) => topMargin + (height - (pitch - minPitch) * noteHeight.toInt)

    // Draw the notes.
    graphics.setPaint(Color.black)
    for (note <- notes) {
      graphics.fillRect(
        pitchX(note.offset.toInt),
        pitchY(note.pitch),
        (note.duration.toFloat * horizontalMultiplier).toInt,
        noteHeight
      )
    }

    // Turn the note indexes to offsets within the tune.
    // Todo: If lookup in notes is O(N) then this could be slow.
    val bracketsWithOnset = brackets.map {
      case (a: Int, b: Int, c: Int, d: Int) => (
        notes(a).offset,
        notes(b).offset + notes(b).duration,
        notes(c).offset,
        notes(d).offset + notes(d).duration
        )}
    graphics.setPaint(Color.DARK_GRAY)
    graphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.1f));

    // Step through colour values in a given range.
    val lowerColourValue = 10
    val upperColourValue = 245
    val colourValueStep = (upperColourValue - lowerColourValue) / (brackets.length + 1)
    var colourValue = lowerColourValue

    for (bracket <- bracketsWithOnset) {
      graphics.setColor(new Color(50, 100, colourValue))

      val firstX = pitchX(bracket._1.toInt)
      val firstWidth = pitchX(bracket._2.toInt) - pitchX(bracket._1.toInt)

      val secondX = pitchX(bracket._3.toInt)
      val secondWidth = pitchX(bracket._4.toInt) - pitchX(bracket._3.toInt)

      // Arc
      val outerRadius =  (secondX + secondWidth) - firstX
      val innerRadius = secondX - (firstX + firstWidth) / 2

      val path = new Path2D.Double()


      /*
      // retired bezier curve code.
      // arcs look cooler and less elephantine.
      // top outer curve, first to second
      path.moveTo(firstX, topMargin.toInt)
      path.curveTo(firstX, topMargin.toInt,
        (secondX - (firstX + firstWidth)) / 2 + firstX + firstWidth, topMargin.toInt - outerRadius,
        secondX + secondWidth, topMargin.toInt
      )

      // highlight second region
      path.lineTo(secondX + secondWidth, pitchY(minPitch-1))
      path.lineTo(secondX, pitchY(minPitch - 1))
      path.lineTo(secondX, topMargin.toInt)

      // inner curve, second to first
      path.curveTo(secondX, topMargin.toInt,
          (secondX - (firstX + firstWidth)) / 2 + firstX + firstWidth, topMargin.toInt - innerRadius,
        firstX + firstWidth, topMargin.toInt
      )

      // highlight second region
      path.lineTo(firstX + firstWidth, topMargin.toInt)

      path.lineTo(firstX + firstWidth, pitchY(minPitch-1))
      path.lineTo(firstX, pitchY(minPitch - 1))
      path.lineTo(firstX, topMargin.toInt)
      */

      // centre of arcs
      val x = (secondX - (firstX + firstWidth)) / 2 + firstX + firstWidth
      val y = topMargin.toInt

      // outer and inner radius
      val r1 = x - firstX;
      val r2 = x - (firstX + firstWidth);

      // diameters of arcs
      val d1 = 2 * r1;
      val d2 = 2 * r2;

      // start angle and extend
      val startAngle = 180;
      val endAngle = -180;
      // create arcs
      val arc1 : Arc2D = new Arc2D.Double(x - r1, y - r1, d1, d1, startAngle, endAngle, Arc2D.OPEN);
      val arc2 : Arc2D = new Arc2D.Double(x - r2, y - r2, d2, d2, startAngle + endAngle, -endAngle, Arc2D.OPEN);

      // a path with two arcs
      path.append(arc1, false);
      path.append(arc2, true);
      path.closePath();

      graphics.fill(path);

      graphics.setColor(new Color(20, 80, colourValue))
      graphics.draw(path);

      colourValue += colourValueStep
    }

    return buffer
  }
}


// MIDI reading utils.
object Midi {

  // Get the MIDI track with give number.
  def getTrackFromMidiFile(location: String, trackNumber: Integer): Option[Track] = {
    val sequencer = MidiSystem.getSequencer()
    if (sequencer == null) {
      return None;
    } else {
      sequencer.open();
      try {
        val myMidiFile: File = new File(location);
        val mySeq: Sequence = MidiSystem.getSequence(myMidiFile);
        val tracks: Array[Track] = mySeq.getTracks();
        if (tracks.length <= trackNumber) {
          None
        }
        else {
          Some(tracks(trackNumber));
        }
      } catch {
        case e: Exception => None
      }
    }
  }

  def structureFromTrack(track: Track): MelodyStructure = {
    val events = for {eventI <- 0 until track.size} yield track.get(eventI);

    // Pitch bit-mask from midi byte.
    val getPitchByte = (ev: MidiEvent) => ev.getMessage.getMessage()(1)

    val notes = (for (event <- events)
    yield event.getMessage.getStatus match {
        // Note on
        case 0x90 => {
          Some(Tuple3[Symbol, Long, Int]('NoteOn, event.getTick, getPitchByte(event)))
        }
        // Note off
        case 0x80 => {
          Some(Tuple3[Symbol, Long, Int]('NoteOff, event.getTick, getPitchByte(event)))
        }
        case default => None
      }).flatMap(_.toList).toList

    def accumulateTimeDelta(inp: List[Tuple3[Symbol, Long, Int]], time: Long = 0): List[Tuple3[Symbol, Long, Int]] = {
      inp match {
        case x :: xs => Tuple3[Symbol, Long, Int](x._1, x._2, x._3) :: accumulateTimeDelta(xs, time + x._2)
        case Nil => Nil
      }
    }

    return new MelodyStructure(accumulateTimeDelta(notes))
  }
}
}
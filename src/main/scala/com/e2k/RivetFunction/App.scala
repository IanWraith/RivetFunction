package com.e2k.RivetFunction

import javax.sound.sampled.AudioSystem
import java.io.File

object App {
  
  def main(args : Array[String]) {
    
    // args[0] - System type
    // args[1] - File name
    
    readWavFile("C:\\temp\\test.wav")
    
    
  }
  
  def readWavFile (fileName : String)	{
  
    val wavFile=new File(fileName)
    val audioInputStream=AudioSystem.getAudioInputStream(wavFile)  
    val bytesPerFrame=audioInputStream.getFormat().getFrameSize()
    val sampleRate=audioInputStream.getFormat().getSampleRate()
    val sampleSizeInBits=audioInputStream.getFormat().getSampleSizeInBits();
    val channels=audioInputStream.getFormat().getChannels();
    val endian=audioInputStream.getFormat().isBigEndian();
       
  }

}

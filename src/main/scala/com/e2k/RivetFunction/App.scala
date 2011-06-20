package com.e2k.RivetFunction

import javax.sound.sampled.AudioSystem
import javax.sound.sampled.AudioInputStream
import java.io.File

object App {
  
  def main(args : Array[String]) {
    
    // args[0] - System type
    // args[1] - File name
     
    val waveData=readWavFile("C:\\temp\\test.wav")
    if (waveData.errorState==true) println ("Error :" + waveData.errorReason)
    else println ("OK !")
    
    
  }
  
  // Read in a WAV file and return a WaveData class
  def readWavFile (fileName : String ): WaveData= {
    val waveData=new WaveData
    
    try	{
    	val wavFile=new File(fileName)
    	val audioInputStream=AudioSystem.getAudioInputStream(wavFile)  
    	waveData.bytesPerFrame=audioInputStream.getFormat().getFrameSize()
    	waveData.sampleRate=audioInputStream.getFormat().getSampleRate()
    	waveData.sampleSizeInBits=audioInputStream.getFormat().getSampleSizeInBits();
    	waveData.channels=audioInputStream.getFormat().getChannels();
    	waveData.endian=audioInputStream.getFormat().isBigEndian();
    	
    	println(waveData.sampleSizeInBits)
    	println(waveData.bytesPerFrame)
    	println(waveData.endian)
    	
    	grabWavBlock(audioInputStream)
    	
    } catch 	{
      case e => waveData.errorReason=e.toString()
      waveData.errorState=true
    }
    return waveData  
  }
  
  // Read in 1024 bytes of a wav file
  def grabWavBlock (audioStream : AudioInputStream) : Array[Integer]=	{
    val initialBlock=new Array[Integer](1024)
    val inBlock=new Array[Byte](1024)
    val count=audioStream.read(inBlock)
    
    
    
    println (inBlock(0))
    println (inBlock(1))
    println (inBlock(2))
    println (inBlock(3))
    println (inBlock(4))
    println (inBlock(5))
    println (inBlock(6))
    println (inBlock(7))
    
    return initialBlock
  }
  

}

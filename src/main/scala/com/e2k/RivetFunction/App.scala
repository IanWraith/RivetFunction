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
    	
    	println ("sampleSizeInBits "+waveData.sampleSizeInBits)
    	
     	grabWavBlock(audioInputStream)
    	
    } catch 	{
      case e => waveData.errorReason=e.toString()
      waveData.errorState=true
    }
    return waveData  
  }
  
  // Read in 2048 bytes of a wav file
  def grabWavBlock (audioStream : AudioInputStream) : Array[Integer]=	{
    val initialBlock=new Array[Integer](1024)
    val inBlock=new Array[Byte](2048)
    val count=audioStream.read(inBlock)
    // Have a loop convert the byte array into an int array
    var i=0
    var a=0
    while (i<count)	{
      initialBlock(a)=LEconv(inBlock(i),inBlock(i+1))
      a=a+1
      i=i+2
    }
    return initialBlock
  }
  
  // Convert from being little endian
  def LEconv (a : Byte , b : Byte) : Integer= 	{
    a&0xFF|b<<8;
  }
  

}

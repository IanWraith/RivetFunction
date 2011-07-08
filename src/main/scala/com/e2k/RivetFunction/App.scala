package com.e2k.RivetFunction

import scala.collection.mutable.ArrayBuffer 
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.AudioInputStream
import java.io.File

object App {
  
  def main(args : Array[String]) {
    
    // args[0] - System Type
    // args[1] - Input File Name
    // args[2] - Output File Name
    
    var displayLines=List[String]() 
    val ret=readWavFile("C:\\temp\\xpa_longish.wav")
    //val ret=readWavFile(args(1))
    val waveData=ret._2
    if (ret._1==true) println ("Error :" + ret._3)
    
    
    //val system=args(1)
    val system="XPA10"
    
    // System choices
    // XPA 10 baud
    if (system=="XPA10")	{
      val xpa=new XPA
      displayLines=xpa.decode(ret._2,10)
      }
    // XPA 20 baud
    else if (system=="XPA20")	{
      val xpa=new XPA
      displayLines=xpa.decode(ret._2,20)
      }
    else {
      println ("Error ! Unknown System")
      return()
    }
   
    // Display the resulting decode info contained in a List
    displayLines.foreach (displayLines => println(displayLines)) 
     
  }
  
  // Read in a WAV file and return a WaveData class
  def readWavFile (fileName : String ): Tuple3 [Boolean,WaveData,String]= {
    val waveData=new WaveData
    var rawData:ArrayBuffer[Int]=new ArrayBuffer() 
    var wavCom=false;
    var errorState=false;
    var errorReason="OK"
    var a=0;
    try	{
    	val wavFile=new File(fileName)
    	val audioInputStream=AudioSystem.getAudioInputStream(wavFile)  
    	waveData.bytesPerFrame=audioInputStream.getFormat().getFrameSize()
    	waveData.sampleRate=audioInputStream.getFormat().getSampleRate()
    	waveData.sampleSizeInBits=audioInputStream.getFormat().getSampleSizeInBits();
    	waveData.channels=audioInputStream.getFormat().getChannels();
    	waveData.endian=audioInputStream.getFormat().isBigEndian();
    	// Keep grabbing 1024 blocks of WAV file until it has all been loaded
    	while (wavCom==false)	{
    		val ret=grabWavBlock(audioInputStream)
    		// Transfer the Int array into a mutuable array buffer 
    		for (a<-0 until ret._1) rawData += ret._2(a)
    		// If less than 1024 Ints are returned this was the last block
    		if (ret._1<1024) wavCom=true
    	}
        // Transfer the completed array buffer into the WaveData objects list
    	waveData.rawList=rawData.clone
    } catch 	{
      case e => errorReason=e.toString()
      errorState=true
    }
    (errorState,waveData,errorReason)  
  }
  
  // Read in 2048 bytes of a wav file
  // Returns a tuple consisting of the block count and an Int array
  def grabWavBlock (audioStream : AudioInputStream ) : Tuple2 [Int,Array[Integer]]= {
    // Decide how to handle the WAV data
    // 16 bit LE
    if ((audioStream.getFormat().isBigEndian()==false)&&(audioStream.getFormat().getSampleSizeInBits()==16)) return grabWavBlock16LE (audioStream) 
    // 8 bit 
    else if (audioStream.getFormat().getSampleSizeInBits()==8) return grabWavBlock8B(audioStream)
    else return (0,Array(0,0))
  }
  
  // Handle 16 bit LE WAV data
  def grabWavBlock16LE (audioStream : AudioInputStream) : Tuple2 [Int,Array[Integer]]= {
    val initialBlock=new Array[Integer](1024)
    val inBlock=new Array[Byte](2048)
    val count=audioStream.read(inBlock)
    // Have a loop convert the byte array into an int array
    var i=0
    var a=0
    for (a<-0 until 1024)	{
      initialBlock(a)=LEconv(inBlock(i),inBlock(i+1))
      i=i+2
    }
    ((count/2),initialBlock)
  }
  
  // Convert from being little endian
  def LEconv (a : Byte , b : Byte) : Integer= 	{
    a&0xFF|b<<8;
  }
  
  // Handle 8 bit WAV files
  def grabWavBlock8B (audioStream : AudioInputStream) : Tuple2 [Int,Array[Integer]]= {
    val initialBlock=new Array[Integer](1024)
    val inBlock=new Array[Byte](1024)
    val count=audioStream.read(inBlock)
    // Have a loop convert the byte array into an int array
    var a=0
    for (a<-0 until 1024) initialBlock(a)=inBlock(a)
    (count,initialBlock)
  }
  

}

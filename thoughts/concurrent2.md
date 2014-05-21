chan1 = BufferedChannel()
chan2 = Channel $ => 
  for i in range(10) do sleep 1 after chan1.send i
chan1.receive $ data => 
  println 'Channel 1 received "#{data}"'
  chan1\buffer.append! data
chan2.dead $ =>
  println "chan2 is dead. Here's what chan1 got:"
  println chan1\buffer
  chan1.kill!

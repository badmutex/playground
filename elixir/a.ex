
defmodule PingPong do

  import :timer

  def spin(name, x, timer) do
    receive do
      {id, msg, n} when n <= x ->
        me = Kernel.inspect self()
        other = Kernel.inspect id
        IO.puts "#{name}/#{me} got #{msg}/#{other}"
        sleep timer
        send id, { self(), name, n+1 }
        spin(name, x, timer)
    end
  end

  def run(n) do

    {ping , _} = spawn_monitor(PingPong, :spin, ["Ping0", n, 500])
    {pong1, _} = spawn_monitor(PingPong, :spin, ["Pong1", n, 1000])
    # {pong2, _} = spawn_monitor(PingPong, :spin, ["Pong2", n, 200])
    # {pong3, _} = spawn_monitor(PingPong, :spin, ["Pong3", n, 700])
    send ping, {pong1, "Pong1", 1}
    # send ping, {pong2, "Pong2", 1}
    # send ping, {pong3, "Pong3", 1}
    
  end

end

PingPong.run 10

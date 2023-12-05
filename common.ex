defmodule Common do
  def read_file(filename) do
    text = File.read!(filename)
    String.split(text, "\n", parts: :infinity, trim: true)
  end
end

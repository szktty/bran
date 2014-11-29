module String = struct
  include String
  include Spotlib.Xstring
  include Xstring
end

module List = struct
  include List
  include Spotlib.Xlist
  include Xlist
end

module Filename = struct
  include Filename
  include Spotlib.Xfilename
end

module Sys = struct
  include Sys
  include Spotlib.Xsys
end

module Unix = struct
  include Unix
  include Spotlib.Xunix
end

module Printf = struct
  include Printf
  include Spotlib.Xprintf
end

% Author: Romain "Artefact2" Dalmaso <artefact2@gmail.com>
%
% This program is free software. It comes without any warranty, to the
% extent permitted by applicable law. You can redistribute it and/or
% modify it under the terms of the Do What The Fuck You Want To Public
% License, Version 2, as published by Sam Hocevar. See
% http://sam.zoy.org/wtfpl/COPYING for more details.

-record(pack, {
	  file_name :: string(),
	  io_device :: file:io_device(),
	  data_offset :: integer(),
	  toc_offset :: integer(),
	  toc_sha1 :: binary(),
	  sha1_table :: ets:tid() | atom(),
	  file_table :: ets:tid() | atom()
}).

-type pack() :: #pack{}.
-export_type([pack/0]).

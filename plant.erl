-module(plant).
-exports([behaviour_info/1]).

behaviour_info(callbacks) ->
	[{loop/1}];
behaviour_info(_Other) ->
	undefined.

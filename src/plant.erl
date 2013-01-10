-module(plant).
-exports([behaviour_info/1]).

behaviour_info(callbacks) ->
	[{init, 1}];
behaviour_info(_Other) ->
	undefined.

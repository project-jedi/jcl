
Diff:   	jpp.dpr \
		JppParser.pas \
		JppState.pas
	diff jpp.dpr ppp.dpr >jpp.diff
	diff JppParser.pas PppParser.pas >JppParser.diff
	diff JppState.pas PppState.pas >JppState.diff

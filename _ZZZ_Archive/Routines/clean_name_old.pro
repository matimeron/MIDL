Function Clean_name_old, name, wild = wld

;+
; NAME:
;		CLEAN_NAME
; VERSION:
;		8.13
; PURPOSE:
;		Cleaning a character string to be used as file name.
; CATEGORY:
;		Character function.
; CALLING SEQUENCE:
;		Result = CLEAN_NAME (NAME)
; INPUTS:
;	NAME
;		Character string.  If not present, defaults to null string.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/WILD
;		Switch.  If set, wild characters ( *, ?) are removed as well.
; OUTPUTS:
;		Returns the original string, with blank spaces removed, with all
;		Hershey codes (of the form '! ') replaced but comas and with all
;		characters used as filename and path separators removed.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls SDEP from IMPORTS and STRPARSE_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 15-FEB-2004 by Mati Meron.
;		Modified 20-NOV-2007 by Mati Meron.  Slight internal changes.
;		Modified 20-NOV-2011 by Mati Meron.  Added keyword WILD.
;-

	on_error, 1

	name = Default(name,'')
	clis = [':','.',sdep(/ds)]
	if keyword_set(wld) then clis = [clis,'*','?']
	res = strcompress(name,/rem)
	n = Strparse_mm(res,'!',lis)
	if n gt 0 then begin
		dum = lis[1:*]
		strput, dum, ',', 0
		lis[1:*] = dum
		res = strjoin(lis)
	endif
	n = Strparse_mm(res,',',lis)
	res = strjoin(lis,',')
	if strpos(res,',',/reverse_off) ge 0 then res=strmid(res,0,strlen(res)-1)
	for i = 0, n_elements(clis) - 1 do begin
		n = Strparse_mm(res,clis[i],lis)
		res = strjoin(lis,' ')
	endfor

	return, res
end
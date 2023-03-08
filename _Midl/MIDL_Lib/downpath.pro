Function Downpath, dir, uplevel = upl, top = top, all_dirs = all, array = arr,$
	count = con

;+
; NAME:
;		DOWNPATH
; VERSION:
;		7.09
; PURPOSE:
;		Generating a search sub-path.
; CATEGORY:
;		Programming utility.
; CALLING SEQUENCE:
;		Result = DOWNPATH( [DIR] [keywords])
; INPUTS:
;	DIR
;		Directory name, optional, defaults to current directory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	UPLEVEL
;		Positive integer or zero, states how many levels above DIR should the
;		path begin.  Defaults to 0.
;	/TOP
;		Switch.  If set, the start point of the path is shifted to the highest
;		possible level.
;	/ALL_DIRS
;		Switch, same as ALL_DIRS in IDL's EXPAND_PATH.  When set, all
;		all directories (not just those including .PRO and .SAV files) are
;		included.
;	/ARRAY
;		Switch, same as ARRAY in IDL's EXPAND_PATH.  When set, the result is
;		returned as a string array (array of directory names) else it is
;		returned as a single string.
;	COUNT
;		Optional output, see below.
; OUTPUTS:
;		Returns
; OPTIONAL OUTPUT PARAMETERS:
;	COUNT
;		The name of the variable in which the number of directories found is
;		returned.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward directory search.  Calls DEFAULT, ONE_OF, STREQ, 
;		STRMATCH_MM and STRPARSE_MM from MIDL and SDEP from Imports.
; MODIFICATION HISTORY:
;		Created 10-SEP-2001 by Mati Meron.
;		Modified 1-JUN-2009 by Mati Meron.  Internal changes.
;-

	on_error, 1

	sd = sdep(/ds)
	cd, cur = cur
	cur = Default(dir,cur)
	upfl = One_of(upl,top)

	if upfl ge 0 then begin
		if Streq(strmid(cur,0,1),sd) then psd = sd else psd = ''
		nd = Strparse_mm(cur,sd,list)
		if upfl eq 0 then nd = nd < (nd- Default(upl,0,/dtyp)) > 1 else nd = 1
		cur = psd + strjoin(list[0:nd],sd)
	endif

	res = expand_path('+' + cur,/array,all=all)
	fostr = '\Folder Settings'
	dum = Strmatch_mm(fostr,res,strlen(fostr),/all,/reverse,/exclude,num=con)
	if keyword_set(arr) then res=res[dum] else res=strjoin(res[dum],sdep(/ps))

	return, res
end
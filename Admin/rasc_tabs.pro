Function Rasc_tabs, filnam, header = hed, _extra = _e

;+
; NAME:
;		RASC_TABS
; VERSION:
;		8.425
; PURPOSE:
;		Reads tab separated ascii files produced EXCEL.
; CATEGORY:
;		I/O
; CALLING SEQUENCE:
;		Result = RASC_TABS( [FILNAM] [, HEADER = HED])
; INPUTS:
;	FILNAM
;		Char. value, the name of the data file.  If not given, the routine will
;		query for it, interactively.  If given and not containing path 
;		information, RASC_TABS will search for the file in the current folder,
;		and only if not found, will query for it.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	HEADER
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to RASCLINE.  Not to be used 
;		directly.  All of RASCLINE keywords can be used.
; OUTPUTS:
;		Returns a 2D character array, where each row contains the entries on
;		one line of the original file, starting with the second line.  The first
;		line is returns separately, in HEADER (see below).
; OPTIONAL OUTPUT PARAMETERS:
;	HEADER
;		Returns a character vector containing the entries on the first line of
;		the original file.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Reads the file using RASCLINE, the corrects for missing entries using
; 		the following rules:
; 			1)	A tab at the beginning of the a line is changed to space + tab
; 			2)  A sequence of tab + tab in the middle of a line is changed to
; 				tab + space + tab.  This rule is applied recursively till no 
; 				double tab sequences remain.
; 			3)	A tab at the end of a line is changed into tab + space.
; 		Following the substitutions the line is parsed using tab (but not space)
; 		as the parsing value.
; 		This process corrects for the fact that when EXCEL writes spreadsheet
; 		data into a text file, null strings are substituted for missing entries.
;		RASC_TABS calls FNAMPARSE, RASCLINE, STREQ, TYPE and STRPARSE_MM, from
;		MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2014 by Mati Meron.
;		Modified 20_AUG-2015 by Mati Meron.  Internal changes to enable 
;		automatic search in current folder.
;-

	on_error, 1
	tab = string(9b)
	bad = tab + tab
	good = tab + ' ' + tab

	if Type(filnam) eq 7 then begin
		fnam = Fnamparse(filnam,pat=pat,ext=ext)
		cur = Streq(pat,'')
		if Streq(ext,'') then wfnam = filnam + '.txt' else wfnam = filnam
	endif

	dum = Rascline(wfnam,lines=lin,count=con,stat=sta,cur=cur,_extra=_e)
	if sta then begin
		for i = 0, con-1 do begin
			tem = lin[i]
			if strmid(tem,0,1) eq tab then tem = ' ' + tem
			repeat begin
				loc = strpos(tem,bad)
				if loc eq -1 then break
				tem = strmid(tem,0,loc) + good + strmid(tem,loc+2)
			endrep until 0
			if strmid(tem,strlen(tem)-1) eq tab then tem = tem + ' '
			lin[i] = tem
			plen = Strparse_mm(lin[i],tab,lis)
			if i eq 0 then begin
				oplen = plen
				check = string(replicate(32b,plen+1))
				isok = replicate(1,con)
				res = strarr(plen+1,con)
			endif
			if lin[i] eq check then isok[i] = 0
			if plen eq oplen then res[*,i] = lis $
			else message, 'Row length discrepancy!'
		endfor
	endif else message, 'Bad or missing file!'

	val = where(isok, con)
	if con gt 1 then begin
		res = res[*,val]
		hed = reform(res[*,0])
		res = res[*,1:*]
		dum = where(res eq ' ',ndum)
		if ndum gt 0 then res[dum] = ''
	endif else message, 'No valid data!'
 
	return, res
end
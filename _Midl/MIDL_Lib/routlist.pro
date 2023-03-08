Pro Routlist, redo = red, nfound = num 

;+
; NAME:
;		ROUTLIST
; VERSION:
;		8.63
; PURPOSE:
;		Generates a full list of routines in the MIDL library.
; CATEGORY:
;		Programming utility.
; CALLING SEQUENCE:
;		ROUTLIST [, /REDO] [, NFOUND = NUM] 
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/REDO
; 		Switch.  If set, the list is generated again.
;	NFOUND
;		Optional output, see below.
; OUTPUTS:
;		None, only populates a common block with the required data.
; OPTIONAL OUTPUT PARAMETERS:
;	NFOUND
;		Returns the number of routines found.
; COMMON BLOCKS:
;		ROUT_INFO.  Contains
;			FLIS	-	A list of all the .PRO files found with full paths.
;			RLIS	-	A list of routine names.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls FNAMPARSE, SORPURGE, STRPARSE_MM and TYPE, from
;		MIDL.  Also calls SDEP from IMPORTS.
; MODIFICATION HISTORY:
;		Created 15-SEP-2014 by Mati Meron.
;		Modified 10-MAY-2018 by Mati Meron.  Internal changes.
;-

	common rout_info, flis, rlis
	on_error, 1

	rdir = 'MIDL'
	adir = '_ZZZ_ARCHIVE'
	gfil = '*.pro'
	num = 0
	ds = sdep(/ds)
	ps = sdep(/ps)

	if keyword_set(red) or Type(flis) eq 0 then begin
		ldir = Strparse_mm(!path,ps,dlis)
		dlis = dlis + ds
		for i = 0l, ldir do begin
			if strpos(strupcase(dlis[i]),ds+rdir+ds) lt 0 then dlis[i] = ''
			if strpos(strupcase(dlis[i]),ds+adir) ge 0 then dlis[i] = ''
		endfor
		dum = where(dlis ne '',nlis)
		dlis = dlis[dum]
	
		flis = []
		for i = 0, nlis-1 do begin
			gnam = dlis[i] + gfil
			plis = file_search(gnam)
			if (size(plis))[0] gt 0 then flis = [flis,plis]
		endfor
	
		num = n_elements(flis)
		rlis = strarr(num)
		for i = 0, num-1 do rlis[i] = Fnamparse(flis[i])
	
		s = Sorpurge(strupcase(rlis),net=num)
		flis = flis[s]
		rlis = rlis[s]
	endif

	return
end
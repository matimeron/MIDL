Pro Osc_read, file, _extra = _e

;+
; NAME:
;		OSC_READ
; VERSION:
;		8.42
; PURPOSE:
;		Reads oscillation data from Liquid Compression file.
; CATEGORY:
;		I/O, Liquid compression specific.
; CALLING SEQUENCE:
;		OSC_READ [,FILE] [,_EXTRA = _E]
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;	FILE
;		The name of the file to be read.  If not given, will be querried for
;		interactively.
; KEYWORD PARAMETERS:
;	_EXTRA
;		A formal keyword used to transfer additional values to imbedded
;		functions.  Not to be used directly.
; OUTPUTS:
; 		No direct output, fills the common block OSC_DAT.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		OSC_DAT.  Contains
;			FNAM	:	The full name of the last file read.
;			T		:	Vector, the time values.
;			A		:	Vector, the area values.
;			P1		:	Vector, the longitudinal pressure values.
;			P2		:	Vector, the transverse pressure values.		
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Straightforward, reads the file, identifies the proper data columns and
; 		puts them in the common block.  Calls FILE_GET, RASCII, STRMATCH_MM and
; 		STRPARSE_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2014 by Mati Meron.
;-

	common osc_dat, fnam, t, a, p1, p2
	on_error, 1

	vnam = ['t','A','P1','P2']
	lnam = strlen(vnam)

	fnam = File_get(file,/read,stat=stat,_extra=_e)
	if stat then begin
		fdat = Rascii(fnam,head=head)
		head = head[-1]
		nhead = Strparse_mm(head,' 	',lis)
		off = (nhead - (size(fdat,/dim))[0]) > 0
		ind = lonarr(4)
		for i = 0, 3 do ind[i] = Strmatch_mm(lis,vnam[i],lnam[i],/case,/nosub)
		ind[[2,3]] = ind[[2,3]] - off
		for i = 0, 3 do dum = $
		execute(vnam[i] + '= reform(fdat[' + string(ind[i],form='(i0)')+ ',*])')
	endif else message, 'Bad filename!'

	return
end
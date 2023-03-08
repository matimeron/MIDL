Function Read_pilatus_info, filename = fnam, length = len, show = sho, $
	stat = sta, nlines = nln, loc = loc, _extra = _e

;+
; NAME:
;		READ_PILATUS_INFO
; VERSION:
;		8.475
; PURPOSE:
;		Reads the detector info imbedded in a Pilatus image file.
; CATEGORY:
;		Data Input/Output.
; CALLING SEQUENCE:
;		Result = READ_PILATUS_INFO( FILENAME = FNAM [, keywords])
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	FILENAME
;		Pilatus data filename.  If not given will be querried for interactively.
;		Serves as both input and output paramater, as on return it contains
;		the full filename used.
;	LENGTH
;		Integer scalar, specifies the length of the data bloc to be searched.
;		Default is 2kB.
;	/SHOW
;		Switch.  If set, the info block (if found) is displayed to the screen.
;	STAT
;		Optional output, see below.
;	NLINES
;		Optional output, see below.
;	LOC
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to FILE_GET.
;		Not to be used directly.
; OUTPUTS:
; 		Returns the info block, if found, as a string array.  If not found,
; 		returns !NULL.
; OPTIONAL OUTPUT PARAMETERS:
;	STAT
;		Integer scalar, success status indicator.  Possible return values are:
;			0	:	No file present.
;			1	:	Success.
;			2	:	File present but no valid info block.
;	NLINES
;		Integer scalar, the number of lines in the info block.
;	LOC
;		2-element integer vector, contains the addresses of the first and last
;		byte of the info block.  For diagnostic purposes only.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data file must exist and contain a valid info block.
; PROCEDURE:
;		Straightforward.  Calls DEFAULT and FILE_GET, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-SEP-2016 by Mati Meron.
;-

	on_error, 1

	fnam = File_get(fnam,stat=sta,_extra=_e)

	pinf = []
	if sta then begin
		tem = bytarr(Default(len,2048l,/dtyp))
		openr, fun, fnam, /get_lun
		readu, fun, tem
		free_lun, fun

		lo = where(tem eq 35, nlo)
		if nlo gt 0 then begin
			good = tem[lo+1] eq 32
			if max(good) ne 0 then begin
				lo = lo[where(good)] + 2
				hi = where(tem eq 10,nhi)
				if nhi gt 0 then begin
					good = tem[hi-1] eq 13
					if max(good) ne 0 then begin
						hi = hi[where(good)] - 2
						nln = n_elements(lo) < n_elements(hi)
						pinf = strarr(nln)
						for i = 0, nln-1 do pinf[i] = string(tem[lo[i]:hi[i]])
						loc = [lo[0]-2,hi[nln-1]+2]
						if keyword_set(sho) then begin
							print
							print, pinf, form = '(a)'
							print							
						endif
					endif else sta = 2
				endif else sta = 2
			endif else sta = 2
		endif else sta = 2
	endif

	return, pinf
end
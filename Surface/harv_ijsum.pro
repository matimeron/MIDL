Pro Harv_ijsum, snum, fnum, ifix = ifx, jfix = jfx, di = di, dj = dj, _extra= _e

;+
; NAME:
;		HARV_IJSUM
; VERSION:
;		7.08
; PURPOSE:
;		Partial summations of PD data.
; CATEGORY:
;		Surface/Pilatus specific.
; CALLING SEQUENCE:
;		HARV_IJSUM, SNUM [,FNUM] , keywords
; INPUTS:
;	SNUM
;		Scan number (only single scans allowed)
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number or a list of frame numbers.  If not given, defaults to all
;		the frames within the scan.
; KEYWORD PARAMETERS:
;	IFIX														|	One and only
;		The fixed I (horizontal coordinate) value to be used.	|	one of these
;	JFIX														|	two can be
;		The fixed J (vertical coordinate) value to be used.		|	used
;
;		Note:	The summation is performed either for fixed I (actually within
;				an interval around the fixed I) and all J, or the other way
;				around.  Accordingly one or the other (but not both) of the two
;				values above must be provided.
;	DI
;		The summation interval for I (delta_I).  If not given, defaults to 1.
;	DJ
;		The summation interval for J (delta_J).  If not given, defaults to 1.
;
;		Note:	Both of the above must be given.  The actual summation integrals
;				used are always odd integers, if any of the inputs is even, it
;				is rounded upward to the nearest odd value.
;	_EXTRA
;		A formal keyword used to trasfer keywords to embedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Screen output only.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the limitations on keywords mentioned above.
; PROCEDURE:
;		Reads in the frame data as a 2D array.  Sms the data in one dimension
;		within a strip around the provided fixed value, then sums the resulting
;		vector within a fixed interval around each point.
;		Calls SCAN_PD_READ and SPEC_FILE_CHECK.  Also calls DEFAULT, MAKE_RANGE
;		and ONE_OF from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JUN-2008 by Mati Meron.
;-

	on_error, 1

	Spec_file_check, snum, /sing, /pildet, list = wsnum
	dat = Scan_PD_read(snum,fnum,/norm,/jimg,_extra=_e)
	dim = (size(dat))[1:2]
	whi = One_of(ifx,jfx,val=fixp)
	fixp = round(fixp)
	hdi = Default(di,0,/dtyp)/2 > 0
	hdj = Default(dj,0,/dtyp)/2 > 0
	if dim[0] le 2*hdi or dim[1] le 2*hdj then message, 'Delta values too big!'
	case whi of
		0	:	begin
					if (fixp-hdi) ge 0 and (fixp+hdi) lt dim[0] then begin
						xtit = 'J_pix'
						len = dim[1]
						wid = 2*hdj + 1
						vec = total(dat[fixp-hdi:fixp+hdi,*],1)
					endif else message, 'Delta_I too big for the given I value!'
				end
		1	:	begin
					if (fixp-hdj) ge 0 and (fixp+hdj) lt dim[1] then begin
						xtit = 'I_pix'
						len = dim[0]
						wid = 2*hdi + 1
						vec = total(dat[*,fixp-hdj:fixp+hdj],2)
					endif else message, 'Delta_J too big for the given J value!'
				end
		else:	message, 'either IFIX or JFIX must be given!'
	endcase

	bas = vec[0:wid-1]
	lo = vec[0:len-wid-1]
	hi = vec[wid:len-1]
	res = total(bas) + total([0,hi-lo],/cum)
	ran = Make_range([wid/2,len- wid/2- 1])

	plot, ran, res, xtit = xtit, ytit = 'counts', _extra = _e

	return
end
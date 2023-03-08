Pro Beam_fields, beam, element= ele, location= loc, parameter = par, $
	code = cod, elset = els, elname = enm, internal = int

;+
; NAME:
;		BEAM_FIELDS
; VERSION:
;		5.6
; PURPOSE:
;		Extract fields from a beam structure of type OPBEAM.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		BEAM_FIELDS, BEAM, [keywords]
; INPUTS:
;	BEAM
;		Structure of type OPBEAM.  See OPBEAM__DEFINE for details.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ELEMENT
;		Optional output, see below.
;	LOCATION
;		Optional output, see below.
;	PARAMETER
;		Optional output, see below.
;	CODE
;		Optional output, see below.
;	ELSET
;		Optional output, see below.
;	ELNAME
;		Optional output, see below.
;	/INTERNAL
;		Switch.  Selects between internal (meter, radian) and external
;		(micron, microradian) units.
; OUTPUTS:
;		None other than the optional outputs through keywords.
; OPTIONAL OUTPUT PARAMETERS:
;	ELEMENT
;		Raturns a character vector, a list of optical elements present in the
;		beam.
;		Note:	If the letter 'x' (or 'y') is appended as the *last* letter
;		of an element name, said alement is active only along the X (or Y)
;		direction.
;	LOCATION
;		Returns a float vector, locations of the optical elements along the
;		beam path, in meters.
;	PARAMETER
;		Returns a float vector, list of parameters pertaining to the optical
;		elements (one parameter per element).
;	CODE
;		Returns an integer vector, list of the optical codes for the elements
;		(one code per element).
;	ELSET
;		Returns an integer vector, specifying the on-off status of the elements,
;		(only 0 and 1 values possible).
;	ELNAME
;		Returns a string vector, a list of the elements' names.
; COMMON BLOCKS:
;		Block BEAM_STUFF, as defined in INIT_OPT.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		BEAM must be a structure of type OPBEAM, containing at least one point
;		beyond the source.
; PROCEDURE:
;		Straightforward.  Calls ARREQ, STRMATCH_MM and VERIFY_STRUCT from MIDL.
; MODIFICATION HISTORY:
;		Created 10-DEC-2006 by Mati Meron.
;-

	common beam_stuff, exs, npomax, mmicr, mwlen
	on_error, 1

	posib = ['fpr','nul','foc','dif','ssl','asl','psr']
	if keyword_set(int) then multps = [1.,0.,1.,1.,1.,1.,1.] $
	else multps = [1.,0.,1.,1/mmicr,1/mmicr,1/mmicr,1.]

	if not Verify_struct(beam,'opbeam') then message, $
	'BEAM must be a scalar structure of type "OPBEAM"!'

	xfl = beam.set and 1
	yfl = beam.set/2 and 1
	nn = beam.npoints
	if nn gt 0 then begin
		inds = 2*lindgen(nn)
		enm = beam.elname[inds]
		if xfl then begin
			xsec = beam.xsec[inds]
			xele = xsec.optel
			xloc = xsec.zval
			xpar = xsec.optpar
			xcod = xsec.optcod
			xels = xsec.elset
		endif
		if yfl then begin
			ysec = beam.ysec[inds]
			yele = ysec.optel
			yloc = ysec.zval
			ypar = ysec.optpar
			ycod = ysec.optcod
			yels = ysec.elset
		endif
	endif else message, 'BEAM not initialized!'

	if xfl and yfl then begin
		if (Arreq(xloc,yloc) and Arreq(xels,yels)) then begin
			loc = xloc
			els = xels
		endif else message, 'Beam corrupted, cannot extract!'
		ele = strarr(nn)
		par = fltarr(nn)
		cod = (inx = (iny = intarr(nn)))
		for i = 0l, nn-1 do begin
			inx[i] = Strmatch_mm(xele[i],posib,3)
			iny[i] = Strmatch_mm(yele[i],posib,3)
		endfor
		dum = where((inx le 0) or (iny le 0) or $
			((inx gt 1) and (iny gt 1) and (inx ne iny)),ndum)
		if ndum gt 0 then message, 'Beam corrupted, cannot extract!'
		for i = 0l, nn-1 do begin
			if inx[i] eq iny[i] then begin
				ele[i] = posib[inx[i]]
				par[i] = xpar[i]*multps[inx[i]]
				cod[i] = xcod[i]
			endif else begin
				if inx[i] gt 1 then begin
					ele[i] = posib[inx[i]] + 'x'
					par[i] = xpar[i]*multps[inx[i]]
					cod[i] = xcod[i]
				endif else begin
					ele[i] = posib[iny[i]] + 'y'
					par[i] = ypar[i]*multps[iny[i]]
					cod[i] = ycod[i]
				endelse
			endelse
		endfor
	endif else begin
		if xfl then begin
			els = xele
			loc = xloc
			par = xpar
			cod = xcod
			els = xels
		endif else begin
			els = yele
			loc = yloc
			par = ypar
			cod = ycod
			els = yels
		endelse
	endelse

	return
end
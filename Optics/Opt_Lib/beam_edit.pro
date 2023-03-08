Function Beam_edit, beam, modify = mdy, value = val, on= on, off= off, $
	set_points = stp, name_points = nmp, before_show = bef, after_show = aft

;+
; NAME:
;		BEAM_EDIT
; VERSION:
;		6.4
; PURPOSE:
;		Modifies beam structures of type OPBEAM.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = Result = BEAM_FIELDS( BEAM, [keywords])
; INPUTS:
;	BEAM
;		Structure of type OPBEAM.  See OPBEAM__DEFINE for details.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	MODIFY
;		A list of optical elements to have their parameters modified.  Can be
;		given either as a numeric list, in which case each entry is the number
;		of the element along the beam structure, starting with the source as #0
;		(note that the source itself is not modifiable), or a list of element
;		names.
;	VALUE
;		Numeric array, a list of the new parameters values for all the elements
;		in the MODIFY list.
;
;		Note:	The two lists, in MODIFY and VALUE, must have same length.
;	ON
;		A list of optical elements to be turned ON, i.e. to have their ELSET
;		field set to 1 (if it is already 1, nothing happens).  Same as in MODIFY
;		the list can be given either as list of element numbers or a list of
;		names.
;	OFF
;		A list of optical elements to be turned OFF, i.e. to have their ELSET
;		field set to 0 (if it is already 0, nothing happens).  Same as in MODIFY
;		the list can be given either as list of element numbers or a list of
;		names.
;	SET_POINTS
;		Numerical array, a list of locations where monitoring points (optical
;		elements of type NULL) are to be added.
;	NAME_POINTS
;		String array, a list of names for the locations set by SET_POINTS.  The
;		default name for all these locations is "NULL_PT".
;	BEFORE_SHOW
;		Prints to the screen, in tabular form, a list o the optical elements and
;		their settings *before* the modification.
;	AFTER_SHOW
;		Same as above, after the modification.
; OUTPUTS:
;		Returns a structute of type OPBEAM, generated through the appropriate
;		modification of the input BEAM.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		Block BEAM_STUFF, as defined in INIT_OPT.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		BEAM must be a structure of type OPBEAM, containing at least one point
;		beyond the source.
; PROCEDURE:
;		Straightforward.  Calls BEAM_FIELDS and BEAM_MAKE.  Calls ISNUM,
;		STRMATCH_MM, TABULATE and VERIFY_STRUCT from MIDL.
; MODIFICATION HISTORY:
;		Created 10-DEC-2006 by Mati Meron.
;		MOdified 10-SEP-2007 by Mati Meron.  Added keyword NAME_POINTS.
;-

	common beam_stuff, exs, npomax, mmicr, mwlen
	on_error, 1

	if not Verify_struct(beam,'opbeam') then message, $
	'BEAM must be a scalar structure of type "OPBEAM"!'

	Beam_fields, beam, element= ele, location= loc, parameter = par, $
	code = cod, elset = els, elname = enm

	if keyword_set(bef) then begin
		print
		Tabulate, enm, ele, loc, par, els, /ind
		print
	endif

	nmd = n_elements(mdy)
	nvl = n_elements(val)
	if nmd gt 0 then begin
		if nmd eq nvl then begin
			if not Isnum(mdy) then begin
				wmdy = lonarr(nmd)
				for i = 0l, nmd-1 do wmdy[i] = Strmatch_mm(mdy[i],enm)
			endif else wmdy = long(mdy)
		endif else message, '"Modify" list size mismatch!'
		if min(wmdy,max=max) le 0 or max ge beam.npoints $
		then message, 'Unrecognizable or out of range for modification!'
		for i = 0l, nmd-1 do par[wmdy[i]] = val[i]
	endif

	non = n_elements(on)
	if non gt 0 then begin
		if not Isnum(on) then begin
			won = lonarr(non)
			for i = 0l, non-1 do won[i] = Strmatch_mm(on[i],enm)
		endif else won = long(on)
		if min(won,max=max) le 0 or max ge beam.npoints $
		then message, 'Unrecognizable or out of range for ON!'
		for i = 0l, non-1 do els[won[i]] = 1
	endif

	noff = n_elements(off)
	if noff gt 0 then begin
		if not Isnum(off) then begin
			woff = lonarr(noff)
			for i = 0l, noff-1 do woff[i] = Strmatch_mm(off[i],enm)
		endif else woff = long(off)
		if min(woff,max=max) le 0 or max ge beam.npoints $
		then message, 'Unrecognizable or out of range for OFF!'
		for i = 0l, noff-1 do els[woff[i]] = 0
	endif

	nsp = n_elements(stp)
	if nsp gt 0 and Isnum(stp) then begin
		pnm = replicate('null_pt',nsp)
		if n_elements(nmp) gt 0 then pnm = ([string(nmp),pnm])[0:nsp-1]
		ele = [ele,replicate('nul',nsp)]
		loc = [loc,stp]
		par = [par,fltarr(nsp)]
		cod = [cod,intarr(nsp)]
		els = [els,1+intarr(nsp)]
		enm = [enm,pnm]
	endif

	ebeam = {opbeam}
	ebeam.name = beam.name
	ebeam.set = beam.set
	ebeam.npoints = 1l
	ebeam.wavl = beam.wavl
	ebeam.elname[0] = beam.elname[0]
	ebeam.xsec[0] = beam.xsec[0]
	ebeam.ysec[0] = beam.ysec[0]

	ebeam = Beam_make(ebeam,ele=ele[1:*],loc=loc[1:*],par=par[1:*],$
			cod=cod[1:*],els=els[1:*],eln=enm[1:*])

	if keyword_set(aft) then begin
		print
		Tabulate, enm, ele, loc, par, els, /ind
		print
	endif

	return, ebeam
end
Function Scan_PD_center, snum, orientation = ori, outmode = otm, $
	rotation = rot, dimensions= dim, status = sta, nscan = nsc, list = lis, $
	_extra = _e

;+
; NAME:
;		SCAN_PD_CENTER
; VERSION:
;		8.475
; PURPOSE:
;		Returns center pixel location of AD frames.
; CATEGORY:
;		SPEC data analysis.
; CALLING SEQUENCE:
;		Result = SCAN_PD_CENTER( SNUM, keywords)
; INPUTS:
;	SNUM
;		A list of scan numbers, in any form recognizable by RANGE_PROC.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ORIENTATION
;		An optional input specifying the detector orientation.  Can be provided
;		as integer input, 0 for horizontal, 1 for vertical.  Alternatively, can
;		also be provided as character input with two possible values, "HOR"
;		(for horizontal) and "VER" (for vertical).  First letter is sufficient.
;		Default is vertical.
;	OUTMODE
;		An optional input specifying the detector column arm used.  Can be
;		provided as integer input, 0 for the "XR" arm, 1 for "GID".
;		Alternatively can be provided as character input with two possible
;		values, "XR" and "GID".  Default is "GID".
;
;		Note:	The default values of these two values are taken from the
;		assigned SPEC file, if one exists.
;
;		Note:	Both keywords above also serve as optional outputs, returning
;		the orientation and detector column arm values actually used.  This is
;		true even if no assigned SPEC file exists, though in such case the
;		values have no effect.
;	ROTATION
;		Both input and optional output.  On input, accepts an integer scalar, 
;		the value of ROTATE (see IDL function ROTATE for details).  If not 
;		given, default value is generated internally.  For output, see Optional
;		Outputs, below.
;	DIMENSIONS
;		Optional output, see below.
;	STATUS
;		Optional output, see below.
;	NSCAN
;		Optional output, see below.
;	LIST
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.
; OUTPUTS:
;		If an assigned SPEC file exists, returns a 2-element vactor containing
;		the X and Y pixel coordinates of the Pilatus' center, corresponding to
;		the defined orientation and output arm mode.  Else returns [-1,-1]
; OPTIONAL OUTPUT PARAMETERS:
;	ROTATION
;		If an assigned SPEC file exists, returns the input value for ROTATE to
;		bring the frame(s) into the proper orientation.
;	DIMENSIONS
;		If an assigned SPEC file exists, returns a 2-element vector containing
;		the X and Y direction of the frame(s) in SNUM.
;	STATUS
;		Returns 1 if an assigned SPEC file exists and has been used, 0 otherwise
;	NSCAN
;		Returns the numbers of scans defined by SNUM.	|	These two are
;	LIST												|	SPEC_FILE_CHECK
;		Returns an array of the scan numbers defined	|	returns.
;		by SNUM.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, reads the data using parameters stored in the FILDAT
;		structure in the common block, if the structure is defined.  Calls
;		SCAN_FIELD_READ and SPEC_FILE_CHECK.  Also calls ARREQ, DEFAULT, 
;		STRMATCH_MM and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-AUG-2008 by Mati Meron.
;		Modified 15-APR-2009 by Mati Meron.  Internal changes.
;		Modified 5-FEB-2010 by Mati Meron.  Internal changes.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Internal changes, APEX related.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 5-FEB-2011 by Mati Meron.  Internal changes.
;		Modified 25-OCT-2011 by Mati Meron.  Internal changes.
;		Modified 25-AUG-2016 by Mati Meron.  Internal changes, Pilatus1M related
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	if n_elements(snum) gt 0 then begin
		Spec_file_check, snum, /pil, nscan = nsc, list = lis, _extra = _e
		if nsc eq 1 then begin
			dori = fix(fildat.scan[lis].pdhv)
			tem = fix(fildat.scan[lis].outmode)
		endif else begin
			dori = fix((Scan_field_read(lis,'pdhv',/const,cfl=cfl))[0])
			if not cfl then message, 'Non constant orientation!'
			tem = fix((Scan_field_read(lis,'outmode',/const,cfl=cfl))[0])
			if not cfl then message, 'Non constant out-arm!'
		endelse
		dotm = 1 - ((tem/2) mod 2)
		sta = 1
	endif else sta = 0

	if Type(ori) eq 7 then ori = abs(Strmatch_mm(ori,['hor','ver'],1,/nosub)) $
	else ori = 0 > Default(ori,Default(dori,1,/dtyp),/dtyp) < 1
	if Type(otm) eq 7 then otm = abs(Strmatch_mm(otm,['XR','GID'],1,/nosub)) $
	else otm = 0 > Default(otm,Default(dotm,1,/dtyp),/dtyp) < 1

	if sta then begin
		pdtyp = Scan_field_read(lis,'pdstat',/const,cfl=cfl)
		if not cfl then message, 'Mixed scans!'
		if not Isnum(rot) then begin
			case pdtyp of
				1	:	rot = 1
				3	:	rot = (2 + 2*otm + ori) mod 4
				5	:	rot = 2
				7	:	rot = 2
				9	:	rot = 0
				11	:	rot = 5
				else:
			endcase
		endif

		dim = shift(Scan_field_read(lis,'pddim',/const,cfl=cfl),rot+(rot ge 4))
		if not Arreq(cfl,[1,1]) then message, 'Non constant dimensions!'
		if nsc eq 1 then begin
			xcen = fix(fildat.scan[lis].pdxc)
			ycen = fix(fildat.scan[lis].pdyc)
		endif else begin
			xcen = fix(Scan_field_read(lis,'pdxc',/const,cfl=cfl))
			if not cfl then message, 'Non constant PDX-center!'
			ycen = fix(Scan_field_read(lis,'pdyc',/const,cfl=cfl))
			if not cfl then message, 'Non constant PDY-center!'
		endelse
		res = shift([xcen,ycen],rot)
		if ((rot+1)/2 and 1) then res[0] = dim[0] - res[0] - 1
		if (rot/2 and 1) then res[1] = dim[1] - res[1] - 1
		if pdtyp eq 5 and res[1] gt dim[1]/2 then begin
			res[0] = dim[0] - res[0] - 1
			res[1] = dim[1] - res[1] - 1
		endif
	endif else begin
		rot = Default(rot,0,/dtyp)
		res = [-1,-1]
	endelse

	return, res
end
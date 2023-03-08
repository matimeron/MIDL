Function Scan_pile_cor, scan, time, tau = tau, _extra= _e

;+
; NAME:
;		SCAN_PILE_COR
; VERSION:
;		4.9
; PURPOSE:
;		Corrects a scan for pileup.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_PILE_COR( SCAN [,TIME] TAU = TAU [, keywords])
; INPUTS:
;	SCAN
;		Either a single valid scan, i.e. a [3,n] array, or a single scan number.
;		In the second case the scan will be read from the currently open SPEC
;		file.  Note that in the second case the appropriate data columns (see
;		SCAN_READ) need to be provided.
; OPTIONAL INPUT PARAMETERS:
;	TIME
;		Numeric vector, or scalar, representing the time over which the
;		corresponding scan points were taken.  Must be of the same length as the
;		data length, or of length 1, in which case it is replicated to the
;		data length.
;
;		Note:	1)	When a scan number is provided, the scan time is being read
;					from the SPEC file.  In this case TIME doesn't need to be
;					provided, but if provided, it overrides the one read
;					internally.
;				2)	When an explicit scan, not a scan number, is given, the
;					TIME input is mandatory.
; KEYWORD PARAMETERS:
;	TAU
;		Scalar, the pileup time, must be in the same units as TIME.
;	_EXTRA
;		A formal key, used to transfer additional keywords to SCAN_READ.  Only
;		active when a scan number, not explicit scan, is given.
; OUTPUTS:
;		Returns an output in a scan format (i.e. a [3,n] array), where the 3
;		columns (in order) are:
;			0	:	X values, unchanged from the original.
;			1	:	Y values, given by the original ones corrected for pileup.
;			2	:	Y errors, calculated form the original ones and the
;					pileup correction.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		If a scan number list is provided, SCAN_READ requires COLUMNS input.
;		See there for details.
; PROCEDURE:
;		Straightforward, prepares the data and feeds it into PILEUP_COR.  Calls
;		SCAN_LIST_VER, SCAN_READ (only when in the list mode) and SCAN_SCALE.
;		Uses CAST, DEFAULT, FPU_FIX, JOIN_XY and SPLIT_XY from MIDL.
; MODIFICATION HISTORY:
;		Created 1-MAR-2004 by Mati Meron.
;		Modified 15-APR-2004 by Mati Meron.  Changed last output column from
;		squared to plain statistical errors.
;-

	on_error, 1

	nsc = Scan_list_ver(scan,flag=lfl,lis=slis)
	if nsc eq 0 then message, 'Missing or inconsistent input!' $
	else if nsc gt 1 then message, 'Only a single scan allowed!'

	if lfl then res = Scan_read(scan,time=tim,_extra=_e) else res = scan
	tim = Default(time,tim)
	len = (size(res))[2]
	if n_elements(tim) eq 1 then tim = Cast(replicate(tim,len),4) $
	else if n_elements(tim) ne len then message, 'TIME size mismatch!'

	if n_elements(tau) eq 0 then message,'Cannot correct without time constant!'
	res = Scan_scale(res,1./tim)
	n = Split_xy(res,x=x,y=y,z=ser)
	yc = Pileup_cor(y,tau)
	serc = (yc/(y*(1 - 2*tau*yc)))*ser
	res = Scan_scale(Join_xy(x,yc,serc),tim)

	return, FPU_fix(res)
end
Function Scan_rfnorm, scan, qc, offset = off, _extra = _e

;+
; NAME:
;		SCAN_RFNORM
; VERSION:
;		8.16
; PURPOSE:
;		Normalizing a reflectivity scan by the calculated Fresnel Reflectivity.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = SCAN_RFNORM( SCAN, QC [, keywords])
; INPUTS:
;	SCAN
;		Either a single valid scan, i.e. a [3,n] array, or a single scan number.
;		In the second case the scan will be read from the currently open SPEC
;		file.  Note that in the second case the appropriate data columns (see
;		SCAN_READ) need to be provided.
;	QC
;		Numeric scalar, real or complex, the reflection Q_c.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	OFFSET
;		Optional offset for the Q_z values (first column), resulting in
;		Q_z --> Q_z + OFFSET.  Default is no offset.
;	_EXTRA
;		A formal key, used to transfer keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns an output in a scan format (i.e. a [3,n] array), where
;		the 3 columns (in order) are:
;			0	:	X values, unchanged from the original.
;			1	:	Y values, given by the original ones divided by the Fresnel
;					reflectivity values corresponding to QC.
;			2	:	Y error values, scaled same as the Y values.
;
;		Note:	The calculation assumes that the first column consists of Q_z
;				values.  No checking is, or can be done.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, uses SCAN_OFFSET and SCAN_SCALE, with the Fresnel
;		reflectivity values being provided by RF from SURF_LIB.
; MODIFICATION HISTORY:
;		Created 20-JUL-2011 by Mati Meron.
;		Modified 1-AUG-2012 by Mati Meron.  Bug fix.
;-

	on_error, 1

	wscan = Scan_offset(scan,xof=off,_extra=_e)
	return, Scan_scale(wscan,1/RF(wscan[0,*],qc),_extra=_e)
end
Pro Save_ref, dat


;+
; NAME:
;		SAVE_REF
; VERSION:
;		4.5
; PURPOSE:
;		Saving reflectivity data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		SAVE_REF, DAT
; INPUTS:
;	DAT
;		Data in the standard [2,*] or [3.*] format.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		A front end to WASCII.  Verifies the data and writes it into a file
;		(querying interactively for a save location if needed).  Calls
;		SCAN_VER from SPEC and WASCII from MIDL.
; MODIFICATION HISTORY:
;		Created 15-AUG-2003 by Mati Meron.
;-

	on_error, 1

	if not Scan_ver(dat) then message, 'Not a valid scan data!' else $
	Wascii, wdat,tit='Reflectivity data',head=['Qz','Reflectivity','Error']

	return
end
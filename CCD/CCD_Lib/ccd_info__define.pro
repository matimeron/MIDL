Pro CCD_info__define

;+
; NAME:
;		CCD_INFO__DEFINE
; VERSION:
;		5.0
; PURPOSE:
;		Defines the {CCD_INFO} substructure used with CCD (Bruker) data
;		processing routines.
; CATEGORY:
;		Initialization.
; CALLING SEQUENCE:
;		None
; INPUTS:
;		None
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
;		Standard.  Defines the structure CCD_INFO, containing relevant CCD
;		frame data, as follows:
;
;			BLKL	:	Length of header block, in bytes (currently 512), long.
;			LINL	:	Length of header line, in bytes (currently 80), long.
;			PIXSIZ	:	Pixel size, in mm, (currently 0.091), float.
;			HEAD	:	A CCD_HEAD structure, containing frame header data.
; MODIFICATION HISTORY:
;		Created 25-JAN-2005 by Mati Meron.
;-

	on_error, 1

	dum = {CCD_info, blkl: 0l, linl: 0l, pixsiz: 0., head: {ccd_head}}

	return
end
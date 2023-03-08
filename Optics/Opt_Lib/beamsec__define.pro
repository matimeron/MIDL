Pro Beamsec__define

;+
; NAME:
;		BEAMSEC__DEFINE
; VERSION:
;		4.2
; PURPOSE:
;		Defines the {BEAMSEC} sub-structure used in optics calculations.
; CATEGORY:
;		Initialization.
; CALLING SEQUENCE:
;		None.
; INPUTS:
;		None.
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
;		Standard.  Defines the structure BEAMSEC, including:
;			BPARS	:	4-element float vector, includes in order, for one
;						transverse direction, the following:
;							0	:	relative intensity.
;							1	:	square of source linear size.
;							2	:	square of source angular size.
;							3	:	current distance from source.  Note that
;									this distance is from the "optical source"
;									which may differ from the physical one.
;			OPTEL	:	String, name of the optical element present at ZLOC.
;						Can be any of:
;							PSR -	Physical source, first element only.
;							NUL	- 	None.  Serves as place holder.
;							FPR -	Free Propagation.
;							FOC	-	Focusing element.
;							DIF	-	Diffusing element.
;							SSL	-	Spatial slit.
;							ASL	-	Angular slit.
;						If the name is followed by the suffix X or Y, this means
;						that the element only acts on the X or Y distribution.
;			ZVAL	:	Float, distance from the starting point.  Note that it
;						does not have to coincide with BPARS[3].
;			OPTPAR	:	Float scalar, the parameter of the optical element.
;			OPTCODE	:	Integer code, specifying some variation on the optical
;						element.
;			ELSET	:	Flag, specifies whether the element is OFF (SET = 0) or
;						ON (SET = 1).
;
;		Note1:	The beam parameters in BPARS are those obtained *after* the
;				application of OPTEL.
;		Note2:	All lengths and angles are given in meters and radians.
; MODIFICATION HISTORY:
;		Created 25-JUL-2001 by Mati Meron.
;		Modified 25-MAY-2002 by Mati Meron.
;		Modified 5-NOV-2006 by Mati Meron.  Added field ELSET.
;-

	on_error, 1

	dum = {beamsec, bpars:fltarr(4),optel:'',zval:0.,optpar:0.,optcod:0,elset:0}

	return
end
Function Water, qz, energy= ekev, wavelength= lamb, dfac= dfac, roughness= roff

;+
; NAME:
;		WATER
; VERSION:
;		4.5
; PURPOSE:
;		Calculating water reflectivity.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = WATER( QZ [ENERGY = EKEV ... WAVELENGTH = LAMB] [, keywords])
; INPUTS:
;	QZ
;		Value(s) of Q_z.  Numeric, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ENERGY
;		Photon energy, in keV.			|	One an only one of these two must
;	WAVELENGTH							|	be provided.
;		Photon wavelength, in Angstrem.	|
;	DFAC
;		Density factor, multiplies the standard water density.  Default is 1.
;	ROUGHNESS
;		Surface roughness, in Angstrem, defaults to 0.
; OUTPUTS:
;		Returns the Fresnel reflectivity value(s) for all QZ given, modified
;		by surface roughness (if given).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Serves as a front end to the function REFLECT, in SRUFF_LIB.  See there.
; MODIFICATION HISTORY:
;		Created 15-AUG-2003 by Mati Meron.
;-

	on_error, 1

	res = Reflect(qz=qz,ener=ekev,wave=lamb,elem=['h','o'],num=[2],dens=1.,$
		dfac=dfac,wei=[2,1],rough=roff,/form)

	return, res
end
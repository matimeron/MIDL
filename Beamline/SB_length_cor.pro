Function SB_length_cor, ene

	on_error, 1

	off = 1.5
	elims = [13.6, 22.2]
	indarr = [[1,1,1],[2,2,0],[4,0,0]]

	res = 0.*ene
	dum = where(ene lt elims[0],ndum)
	if ndum gt 0 then res[dum] = $
	Bragg_angle(ene=ene[dum],crys='si',ind=indarr[*,0],/rad)
	dum = where(ene ge elims[0] and ene lt elims[1],ndum)
	if ndum gt 0 then res[dum] = $
	Bragg_angle(ene=ene[dum],crys='si',ind=indarr[*,1],/rad)
	dum = where(ene ge elims[1],ndum)
	if ndum gt 0 then res[dum] = $
	Bragg_angle(ene=ene[dum],crys='si',ind=indarr[*,2],/rad)
	res = off*tan(res)

	return, res
end
		
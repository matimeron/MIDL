Function Nano_ELD, corsize, fulsize, pack_frac = paf, step = stp, energy = ene,$
	core = cor, ligant = lig, substrate = sub, dcor= dco, dlig= dli, dsub= dsu,$
	density = den, edensity = eld, tanphi = tph, dielect = diel, show = sho

	common sxr_stuff, sorlist, abctab, enun, denun, curbeam

	on_error, 1
	dnst = 3

	Chemparse, cor, ele = cele, wei = cwei
	Chemparse, lig, ele = lele, wei = lwei
	if Type(sub) ne 7 then begin
		sele = ['H','O']
		swei = [2*1.008,16.]/total([2*1.008,16.])
	endif else Chemparse, sub, ele = sele, wei = swei
	elem = [cele,lele,sele]
	nel = [n_elements(cele),n_elements(lele),n_elements(sele)]
	cnel = total(nel,/cum,/pres)

	dco = Default(dco,0)
	if dco eq 0 then if nel[0] eq 1 then dco = abctab[Elecomp(cele)].ro $
	else message, 'Missing core density!' 
	dsu = Default(dsu,1.,/dtyp)

	rcor = corsize/2.
	rful = Default(fulsize,corsize)/2. > rcor
	wpaf = Default(paf,1.,/dtyp) < 1
	if Isnum(stp) then nst1 = (ceil(2*rcor/stp)/2*2 + 1) > dnst else nst1 = dnst
	wst1 = 2*rcor/((nst1-1) > 1)

	if rful gt rcor then begin
		nst2 = ceil((rful-rcor)/wst1 + 1)
		z = Var_grid([-rful,-rcor,rcor,rful],[nst2,nst1,nst2],len=n)
	endif else z = Var_grid([-rcor,rcor],nst1,len=n)
	z = [z,2*z[-1]-z[-2]]

	zlo = z[0:-2]
	zhi = z[1:-1]
	dz = zhi - zlo
	ssum = zlo^2 + zlo*zhi + zhi^2

	mskco = (mskli = (msksu = lonarr(n)))
	dum = where((zlo ge -rcor) and (zhi le rcor), ndum)
	if ndum gt 0 then mskco[dum] = 1
	dum = where((zlo ge -rful) and (zhi le rful), ndum)
	if ndum gt 0 then mskli[dum] = 1
	msksu[n-1] = 1

	vtot = 2*sqrt(3)*rful^2*dz/wpaf
	vco = !pi/3*dz*(3*rcor^2 - ssum)*mskco
	vli = (!pi/3*dz*(3*rful^2 - ssum) - vco)*mskli
	vsu = (vtot - vco - vli)*msksu

	den = (vco*dco + vli*dli + vsu*dsu)/vtot

	warr = fltarr(total(nel,/pres),n)
	warr[0:cnel[0]-1,*] = dco*vco/vtot##cwei
	warr[cnel[0]:cnel[1]-1,*] = dli*vli/vtot##lwei
	warr[cnel[1]:cnel[2]-1,*] = dsu*vsu/vtot##swei

	eld = (tph = fltarr(n))
	diel = complexarr(n)
	wene = Default(ene,10.,/dtyp)
	for i = 0, n-1 do begin
		eld[i] = Electron_density(elem=elem,den=den[i],wei=reform(warr[*,i]))
		diel[i] = Dielect(wene,elem=elem,den=den[i],wei=reform(warr[*,i]))
		tph[i] = Imaginary_mm(diel[i])/Real_mm(diel[i])
	endfor

	z = z[0:n-1] - z[0]
	if keyword_set(sho) then begin
		shoz = [z,z[1:*] - 2*Toler()*max(abs(z)),2*z[-1]-z[-2]]
		shoz = shoz[sort(shoz)]
		shod = transpose([[eld],[eld]])/eld[-1]
		shod = reform(shod,2*n)
		plot, shoz, shod
	endif
	return, z
end
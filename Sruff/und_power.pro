Pro UND_power, cur, dist, k, lamb, rgam, nper, rsig, asig, file = fname, $
	ang_lims = als, npoints = np, gscale = gsca, smear = sme, window = win, $
	tilt_ang = tang, xtilt = xti, degrees = deg, levels = slev, $
	contour= con, surface= sur, outpower = opow

	common un_consts, hcove, alpha, oovee, ecf

	on_error, 1
	u = UN_struct(npo_max = nmx)

	cur = Default(cur,1e-3,/dtype)
	dist = Default(dist,1.,/dtype)

	if n_elements(fname) ne 0 then begin
		fname = $
		file_get(fname,filt='dat',path=getenv('sruff_data'),/pick,stat=stat)
		if stat then begin
			openr, datun, fname, /get_lun, /block
			readu, datun, u
			free_lun, datun
		endif else message, 'Cannot find file!'
	endif else u = Unpri_sto(u,k,lamb,rgam,nper)

	u = Unsec_sto(u,rsig,asig,ang=als,npo=np,xyg=angs,fun=glob,gsc=gsca,/new)
	x = dist*u.ganx(0:2*u.nxy[0])
	y = dist*u.gany(0:2*u.nxy[1])
	arel = abs((x[1]-x[0])*(y[1]-y[0]))
	angs = 1e-3*u.gamm*angs

	ks = 1. + Cast(u.k,4)^2
	coef = 3./(16*!pi)*(5+ks*(1+ks*(3+ks*7)))/ks^(7./2.)

	for i = 0l, u.nxy[0] do begin
		for j = 0l, u.nxy[1] do begin
			glob(i,j) = KJK_fun(u.k,angs(0,i,j),angs(1,i,j))
		endfor
	endfor
	glob[0:u.nxy[0],u.nxy[1]+1:2*u.nxy[1]] = $
	rotate(glob[0:u.nxy[0],0:u.nxy[1]-1],7)
	glob[u.nxy[0]+1:2*u.nxy[0],*] = rotate(glob[0:u.nxy[0]-1,*],5)

	if keyword_set(sme) then glob = Smear(glob,u,dist,smearst=smest)
	glob = glob*Winframe(x,y,window=win,xygrid=wtem,windex=qxy,fringe=fri)
	if n_elements(tang) ne 0 then glob = glob* $
	Tilt(tang,deg=deg,xve=x,yve=y,are=arel,xti=xti,tiltst=tilst)

	totpcal = 2*!pi/3*alpha*hcove*(u.k*u.gamm)^2*u.nper*cur/u.lamb
	glob = 1e-6*totpcal*coef*(u.gamm/dist)^2*glob
	totp = arel*Partot(glob,syme=qxy,symf=fri)

	Gentit, u, cur, dist, quan = 'area', total = totp, peak = max(glob), $
	smear = smest, tilt = tilst, surface = surf, $
	title = tit, subtitle = subtit, xtit = xtit, ytit = ytit, ztit = ztit
	tit = tit[0] + tit[1] + 'Analytical result.  Total calculated power =' + $
	strcompress(string(totpcal, form = '(g10.4)')) + ' Watt'
	subtit = subtit[0] + subtit[1]

	if keyword_set(con) then $
		levs = Conlevs(glob,/ignore,annot=annot,lines=lin,show=slev)

	Und_display,glob, x, y, con=keyword_set(con), sur=keyword_set(sur),$
	levels = levs, c_line = lin, c_annotation = annot, $
	tit=tit, subtit=subtit, xtit=xtit, ytit=ytit, ztit=ztit, _extra=_e

	opow = make_array(3,2*u.nxy[0] + 1, 2*u.nxy[1] + 1, /float)
	opow[0:1,*,*] = wtem
	opow[2,*,*] = glob

	return
end
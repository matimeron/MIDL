Function Unsec_sto, u, rsig, asig, ang_lims = als, npoints = np, newxy = new, $
    xygrid = xyg, fungrid = fung, gscale = gsca

    on_error, 1
    acon = 1e-3*u.gamm
    if keyword_set(gsca) then iacon = 1./acon else iacon = 1.

    if n_elements(rsig) eq 1 then u.rsig = [rsig,rsig] else $
	u.rsig = Default(rsig,u.rsig)
    if n_elements(asig) eq 1 then u.asig = iacon*[asig,asig] else $
	u.asig = iacon*Default(asig,u.asig/iacon)

    if How_many(first = als, second = np, third = new) gt 0 then begin
	if not Arreq(u.nxy, [0l,0l]) then begin
	    unp = u.nxy
	    uals = [[u.ganx([0l,2*u.nxy(0)])],[u.gany([0l,2*u.nxy(1)])]]
	endif

	als = iacon*Default(als,Default(uals,1.5/acon*[u.k> 2, 2])/iacon)
	if n_elements(als) eq 2 then als = [-1,1]#transpose(als)
	np = $
	Default(np,Default(unp,round(16*acon*transpose(als(1,*))),/dtyp),/dtyp)
	if n_elements(np) eq 1 then np = round(np*[(u.k > 2)/2.,1])
	np = np < [n_elements(u.ganx),n_elements(u.gany)]/2

	u.nxy = np
	xyg = Make_grid(als, 2*u.nxy + 1, fun = fung)
	u.ganx(0:2*u.nxy(0)) = reform(xyg(0,*,0))
	u.gany(0:2*u.nxy(1)) = reform(xyg(1,0,*))
    endif

    return, u
end

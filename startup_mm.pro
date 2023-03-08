	!quiet = 1
;	!order = 1
	ds = sdep(/ds)
	tfil = file_which('startup_mm.pro')
	num = Strparse_mm(tfil,ds,lis)
	sroot = ''
	for i = 0, num-1 do sroot = sroot + lis[i] + ds
	if strpos(sroot,':') lt 0 then sroot = ds + ds + sroot
	sruff_dir = sroot + 'sruff' + ds
	sruff_data = sruff_dir + 'data' + ds
	mono_dir = sroot + 'mono' + ds
	mono_data = mono_dir + 'data' + ds
	setenv, 'root_mm=' + sroot
	setenv, 'sruff_dir=' + sruff_dir
	setenv, 'sruff_data=' + sruff_data
	setenv, 'mono_dir=' + mono_dir
	setenv, 'mono_data=' + mono_data
	Constants, /cgs
	SR_constants
	BL_pars
	Pcols
	Psyms
	Rainbow
	Rainbow_mm
	Screen
	dum = FPU_fix((sqrt((machar()).xmin)/10)^2)
;	if not Streq(lis[0],'X',1) then stop
;	@g:\idl_user\idl_startup.pro
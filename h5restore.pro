; hdf5_datafile::restore
;   
pro h5restore, vars, filename=filename, group=group, names=names, except=except, write=write, objects=objects, variables=variables, h5file_obj_name=h5file_obj_name, quiet=quiet
    compile_opt idl2, hidden
    on_error, 2
    
    ; datasets larger than this threshold will be returned as h5variable objects
    ; unless the VARIABLES keyword is set.  Currently set to 10MB.  Datasets 
    ; that are >8GB are always restored as objects.
    if ~keyword_set(variables) then begin
        size_threshold = 10. * 1024.^2
    endif else begin
        size_threshold = 8*1024.^3
    endelse

    if n_elements(filename) eq 0 then $
        filename = dialog_pickfile(filter="*.hdf5;*.h5;*.hdf")
    if filename eq '' then $
        return
    savefile = obj_new('h5file', filename, group=group, read=~keyword_set(write))

    if n_elements(vars) eq 0 then $
        vars = savefile->get_names()
    if n_elements(names) eq 0 then $
        names = strarr(n_elements(vars))
    if n_elements(names) ne n_elements(vars) then $
        message, 'NAMES must be the same length as the number of variables to be restored.'
    if n_elements(except) eq 0 then $
        except = ['']
    except = strupcase(except)
        
    opened_objects = 0
    for i = 0, n_elements(vars) - 1 do begin
        if where(except eq strupcase(vars[i])) eq -1 then begin 
            dataset = savefile->get(vars[i])
            print_string = 'Restoring "' + vars[i] 
            if names[i] ne '' then name = names[i] else name = vars[i]
            idlname = idl_validname(name, /convert_all)
            if keyword_set(objects) || (dataset->size() gt size_threshold) then begin
                if ~ keyword_set(quiet) then print, print_string + '" as a h5variable object...'
                (scope_varfetch(idlname, /enter, level=-1)) = dataset
                opened_objects = 1
            endif else begin
                if ~ keyword_set(quiet) then print, print_string + '" as an IDL variable...'
                (scope_varfetch(idlname, /enter, level=-1)) = dataset->read()
                obj_destroy, dataset
            endelse
        endif
    endfor

    if opened_objects then begin
        if n_elements(h5file_obj_name) eq 0 then $
            h5file_obj_name = 'H5FILE'
        (scope_varfetch(h5file_obj_name, /enter, level=-1)) = savefile
        print, 'H5File object is named ' + h5file_obj_name + '.'
    endif else begin
        obj_destroy, savefile
    endelse
end

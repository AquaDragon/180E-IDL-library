;docformat = 'rst'

; h5file
;
; Based on H5VAR code by Andrew Collette
;
;----------------------------------------------------------------------------

;+
; Creates a new h5file object
;
; :Params:
;    filename : in, optional, type=string
;       Name of file to open.  Not necessary if /pick is set, or you intend
;       to call h5file->open later on.
;
; :Keywords:
;    pick : in, optional, type=boolean
;        Open a file selection dialog
;    create : in, optional, type=boolean
;        Specify that a new file should be opened
;    read : in, optional, type=boolean
;        Require that the file be opened in read-only mode
;-
function h5file::Init, filename, pick=pick, _extra=extra
    compile_opt idl2, hidden
    on_error, 2

    if keyword_set(pick) then begin
        filename_out = DIALOG_PICKFILE(filter=['*.hdf5','*.h5'])
        if(~file_test(filename_out)) then message, 'No file selected.'
    endif else begin
        if(n_elements(filename) eq 0) then message, 'No file specified.'
        filename_out = filename
    endelse

    self->open, filename_out,  _extra=extra

    ; this should be static
    self.types = [ 'UNDEFINED', 'BYTE', 'INT', 'LONG', 'FLOAT', $
                 'DOUBLE', 'COMPLEX', 'STRING', 'STRUCT', 'DCOMPLEX', $
                 'POINTER', 'OBJREF', 'UINT', 'ULONG', 'LONG64', 'ULONG64' ]
    return, 1
end

;----------------------------------------------------------------------------
;+
; Open the specified file
;
; :Params:
;   filename : in, required, type=string
;
; :Keywords:
;   pick : in, optional, type=boolean
;       Open a file selection dialog
;   create : in, optional, type=boolean
;       Specify that a new file should be opened
;   read : in, optional, type=boolean
;       Require that the file be opened in read-only mode
;-
pro h5file::open, filename, pick=pick, group=group, create=create, read=read
    compile_opt idl2, hidden
    on_error, 2

    self.filename=filename
    if file_test(filename) then begin
        if keyword_set(create) then begin
            message, 'File already exists. Exiting.'
        endif

        if ~file_test(filename, /regular) then begin
            message, 'Directory or special file with that name already exists. Exiting.'
        endif

        if ~file_test(filename, /read) then begin
            message, 'Could not open file. Exiting.'
        endif
    
        if ~h5f_is_hdf5(filename) then begin
            message, 'File exists but is not a valid HDF5 file. Exiting.'
        endif

        if file_test(filename, /write) && ~keyword_set(read) then begin
            self.file_id = h5f_open(filename, /write)
            self.readonly=0
        endif else begin
            self.file_id = h5f_open(filename)
            self.readonly=1
        endelse
        if n_elements(group) eq 0 then $
            group = '/'
        self.group_id = h5g_open(self.file_id, group)

    endif else begin
        if keyword_set(read) then begin
            message, 'File does not exist.'
        endif
        self.file_id = h5f_create(filename)
        if n_elements(group) eq 0 then begin
            self.group_id = h5g_open(self.file_id, '/')
        endif else begin
            self.group_id = h5g_create(self.file_id, group)
        endelse
   endelse
end

;----------------------------------------------------------------------------

;+
; Internal cleanup routine
;
; :Hidden:
;-
pro h5file::Cleanup
    compile_opt idl2, hidden
    on_error, 2
    self->close
end

;----------------------------------------------------------------------------

;+
; Close the current file
;-
pro h5file::close
    compile_opt idl2, hidden
    on_error, 2

    h5g_close, self.group_id
    h5f_close, self.file_id
end

;----------------------------------------------------------------------------

;+
; Check to see if "name" corresponds to a valid dataset in the file, using 
; case-insensitive comparison.
;
; :Params:
;   name : in, required, type=string
;       Name of HDF5 dataset
;
; :Keywords:
;   realname : in, optional, type=string
;       The name of the corresponding dataset in the file (in the correct case)
;       will be copied into this variable
;
; :Returns: (int) 1 if the dataset exists, 0 otherwise
;
;-
function h5file::is_dataset, name, realname=realname
    compile_opt idl2, hidden
    on_error, 2

    n_members = h5g_get_num_objs(self.group_id)
    for i = 0, n_members - 1 do begin
        member_name = h5g_get_obj_name_by_idx(self.group_id, i)
        member_info = h5g_get_objinfo(self.group_id, member_name)
        if (member_info.type eq 'DATASET') && (strupcase(name) eq strupcase(member_name)) then begin
            realname=member_name
            return, 1
        endif
    endfor

    return, 0
end

;----------------------------------------------------------------------------

;+
; Return an h5variable object corresponding to the specified dataset
; The returned object will be read-only if the current HDF5 file has been
; opened in read-only mode.
; 
; :Params:
;   name : in, required, type=string
;       Name of the dataset to get.  Case-insensitive.
;
; :Returns: h5variable object
;-
function h5file::get, name
    compile_opt idl2, hidden
    on_error, 2
    realname=''
    if ~self->is_dataset(name, realname=realname) then $
        message, 'Dataset "' + name + '" is not present in this file.'
    return, obj_new('h5variable', self.group_id, realname, readonly=self.readonly)
end

;----------------------------------------------------------------------------

;+
; Create a new dataset with the specified name.  You may specify either:
; ::
;   (1) An IDL array (param "data")
;   (2) Both keywords "dimensions" *and* "type"
;
; :Params:
;   name : in, required, type=string
;       Name of the dataset to create.  Case-preserving.
;   data : in, optional, type="<any supported IDL type>"
;       IDL variable to store in the dataset.
;
; :Keywords:
;   _extra : in, optional, type="keywords"
;       All keywords supported by h5variable::Init
;-
function h5file::create, name, data, _extra=extra
    compile_opt idl2, hidden
    on_error, 2
    if self.readonly then $
        message, 'Cannot create dataset because this data file is read-only.'
    if self->is_dataset(name) then $
        message, 'A dataset named "' + name + '" is already present in this file.'
    return, obj_new('h5variable', self.group_id, name, data, /create, _extra=extra)
end

;----------------------------------------------------------------------------

;+
; Read directly from the specified dataset
;
; :Returns: IDL variable
;
; :Params:
;   name : in, required, type=string
;       Name of dataset to open (case-insensitive)
;   start: in, optional, type="1D intarr"
;       HDF5 starting offset for read
;   length: in, optional, type="1D intarr"
;       HDF5 length offset for read
;   stride: in, optional, type="1D intarr"
;       HDF5 stride for read
;
; :Keywords:
;   _extra : in, optional, type="keywords"
;       All keywords supported by h5variable::read
;
; :History:  A.C. 12/12/07
;-
function h5file::read, name, start, length, stride, _extra=extra
    compile_opt idl2, hidden
    on_error, 2

    ds_obj = self->get(name)
    ds = ds_obj->read(start, length, stride, _extra=extra)
    obj_destroy, ds_obj
    return, ds

end

;----------------------------------------------------------------------------

;+
; Write directly to the specified dataset
;
; :Params:
;   name : in, required, type=string
;       Name of dataset to open (case-insensitive)
;   data : in, required, type="<any supported IDL type>"
;       IDL data to write
;   start: in, optional, type="1D intarr"
;       HDF5 starting offset for write
;   length: in, optional, type="1D intarr"
;       HDF5 length offset for write
;   stride: in, optional, type="1D intarr"
;       HDF5 stride for write
;
; :Keywords:
;   _extra : in, optional, type="keywords"
;       All keywords supported by h5variable::write
;
; :History:  A.C. 12/12/07
;-
pro h5file::write, name, data, start, length, stride, _extra=extra
    compile_opt idl2, hidden
    on_error, 2

    ds_obj = self->get(name)
    ds_obj->write, data, start, length, stride, _extra=extra
    obj_destroy, ds_obj

end

;----------------------------------------------------------------------------

;+
; Prints a short informative message about the file contents to stdout
;-
pro h5file::ls
    compile_opt idl2, hidden
    on_error, 2
    print, 'Datasets in "' + self.filename + '"'
    print, '-------------' + strjoin(replicate('-', strlen(self.filename) + 1))

    n_members = h5g_get_num_objs(self.group_id)
    if n_members eq 0 then begin
        print, '<none>'
    endif
    for i = 0, n_members - 1 do begin
        member_name = h5g_get_obj_name_by_idx(self.group_id, i)
        member_info = h5g_get_objinfo(self.group_id, member_name)
        if (member_info.type eq 'DATASET') then begin
            dataset_id = h5d_open(self.group_id, member_name)
            type_id = h5d_get_type(dataset_id)
            space_id = h5d_get_space(dataset_id)
            typename = self.types[h5t_idltype(type_id)]
            h5s_select_all, space_id
            dims = strjoin(string(h5s_get_simple_extent_dims(space_id), format='(i0)'), ', ')
            print, member_name + ': ' + typename + ' ' + '[' + dims +']'
        endif
    endfor
end

;----------------------------------------------------------------------------

;+
; Gets a list of dataset names
;
; :Returns: strarr
;-
function h5file::get_names
    compile_opt idl2, hidden
    on_error, 2

    n_members = h5g_get_num_objs(self.group_id)
    names = strarr(n_members)
    for i = 0, n_members - 1 do begin
        member_name = h5g_get_obj_name_by_idx(self.group_id, i)
        member_info = h5g_get_objinfo(self.group_id, member_name)
        if (member_info.type eq 'DATASET') then begin
            names[i] = member_name
        endif
    endfor
    return, names
end

;----------------------------------------------------------------------------

;+
; :Fields:
;   filename  String containing current filename
;   file_id  HDF5 library ID for current file
;   group_id  HDF5 library ID for root group
;   readonly  Int (1=ro, 0=rw)
;   types  strarr containing a list of valid types (for ls routine)
;-
pro h5file__define
    compile_opt idl2, hidden
    struct = { h5file, $
               filename: '', $
               file_id: long(0), $
               group_id: long(0), $
               readonly: fix(0), $
               ; this should be static
               types: strarr(16) $
             }
end
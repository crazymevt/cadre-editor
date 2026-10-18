import os, pathlib, subprocess, tempfile, time, shutil
project = pathlib.Path(__file__).resolve().parent.parent
root = pathlib.Path(tempfile.mkdtemp(prefix='cadre-recovery-crash-'))
state = root / 'state'
environment = dict(os.environ, XDG_CONFIG_HOME=str(root / 'config'), XDG_STATE_HOME=str(root / 'state-home'))
source = root / 'source.lisp'
source.write_text('newer disk contents')
ready = root / 'ready'
setup = '''(push (truename ".") asdf:*central-registry*)
(dolist (name '("../gtk4/gtk4.asd" "../docset/docset.asd"))
  (when (probe-file name) (push (uiop:pathname-directory-pathname (probe-file name)) asdf:*central-registry*)))
(ql:quickload :cadre :silent t)
'''+f'(defun cadre-ui::state-directory () #P"{state}/")\n'
child_file = root / 'child.lisp'
child_file.write_text(setup + f'''
(let ((buffer (cadre:make-buffer :file #P"{source}" :text (cadre:make-string-text "unsaved crash edits"))))
  (setf (cadre:buffer-local buffer :checkpoint-attached) t (cadre:buffer-modified-p buffer) t)
  (cadre-ui::checkpoint-buffer buffer))
(with-open-file (out #P"{ready}" :direction :output) (write-string "ready" out))
(loop (sleep 1))
''')
verify = root / 'verify.lisp'
verify.write_text(setup + '''
(let ((entries (cadre-ui::recoverable-snapshots)))
  (assert (= 1 (length entries)))
  (assert (equal "unsaved crash edits" (getf (cdar entries) :text))))
(format t "Actual SIGKILL recovery check passed.~%")
''')
child = None
try:
    with (root / 'child.log').open('w') as log:
        child = subprocess.Popen(['sbcl', '--non-interactive', '--load', str(child_file)], cwd=project, env=environment, stdout=log, stderr=subprocess.STDOUT)
        deadline = time.monotonic() + 30
        while not ready.exists() and child.poll() is None and time.monotonic() < deadline:
            time.sleep(.1)
        if not ready.exists():
            raise RuntimeError((root / 'child.log').read_text())
        child.kill()
        child.wait(timeout=10)
    result = subprocess.run(['sbcl', '--non-interactive', '--load', str(verify)], cwd=project, env=environment, text=True, capture_output=True, timeout=30)
    if result.returncode:
        raise RuntimeError(result.stdout + result.stderr)
    assert source.read_text() == 'newer disk contents'
    print(result.stdout.splitlines()[-1])
finally:
    if child is not None and child.poll() is None:
        child.kill()
        child.wait()
    shutil.rmtree(root)

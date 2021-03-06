# This little script compiles a target LaTeX file into a PDF using
# latexmk and (by default) lualatex. This is handy when you want to
# compile a simple document without setting up a Makefile. Output
# files are written to your $TMPDIR while the final PDF is copied to
# your current working directory.
#
# Usage:
#   quicklatexmk [latex-opts] FILE
#
# [latex-opts] are passed to latexmk's `-pdflatex=` argument. By
# default these are:
#   lualatex --file-line-error --interaction=nonstopmode
# If, for example, you'd rather use xelatex, you could run:
#   quicklatexmk xelatex FILE

function quicklatexmk -d "Quickly compile a latex file using latexmk"
  if [ (count $argv) -lt 1 ]
    printf "Usage:\n \t quicklatexmk [latex-options] FILE\n" 
    return 1
  end

  set -l FILE_NAME
  set -l LATEX_ARGS "lualatex --file-line-error --interaction=nonstopmode"

  # Parse arguments
  if [ (count $argv) -eq 1 ]
    set FILE_NAME $argv[1]
  else
    set FILE_NAME $argv[-1]
    set LATEX_ARGS $argv[1..-2]
  end

  if not [ -e "$FILE_NAME" ]
    printf "$FILE_NAME does not exist\n"
    return 1
  end

  # Create temporary directory for LaTeX output
  set -l OUTPUT_DIR "$TMPDIR/$FILE_NAME""_Output"
  echo $OUTPUT_DIR
  if not [ -d "$OUTPUT_DIR" ]
    mkdir "$OUTPUT_DIR"
    if [ $status -ne 0 ]
      printf "Unable to create output directory\n"
      return 1
    end
  end

  latexmk -g -shell-escape -outdir="$OUTPUT_DIR" -pdf -pdflatex="$LATEX_ARGS" "$FILE_NAME"

  if [ $status -ne 0 ]
    return 1
  end

  # Copy the compiled PDF from the temporary directory to the current
  # working directory
  set -l FILE_BASENAME (basename $FILE_NAME | sed 's/\.[^.]*$//')
  set -l PDF_OUTPUT "$OUTPUT_DIR/$FILE_BASENAME.pdf"
  cp "$PDF_OUTPUT" .
end

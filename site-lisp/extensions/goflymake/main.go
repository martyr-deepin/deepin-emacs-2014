package main

import (
	"flag"
	"fmt"
	"go/build"
	"log"
	"os"
	"os/exec"
	"path"
	"strings"
)

const (
	testSuffix = "_test.go"
)

var (
	prefix = flag.String("prefix", "flymake_", "The prefix for generated Flymake artifacts.")
	debug  = flag.Bool("debug", false, "Enable extra diagnostic output to determine why errors are occurring.")

	testArguments  = []string{"test", "-c"}
	buildArguments = []string{"build", "-o", "/dev/null"}
)

func main() {
	flag.Parse()
	goflymakeArguments := flag.Args()

	if len(goflymakeArguments) != 1 {
		log.Fatalf("%s %ssome_file.go", path.Base(os.Args[0]), *prefix)
	}

	file := goflymakeArguments[0]
	base := path.Base(file)

	if !strings.HasPrefix(base, *prefix) {
		log.Fatalf("%s lacks the appropriate filename prefix %s", base, *prefix)
	}

	orig := base[len(*prefix):]
	isTest := false
	var goArguments []string

	if strings.HasSuffix(orig, testSuffix) {
		isTest = true
		// shame there is no '-o' option
		goArguments = append(goArguments, testArguments...)
	} else {
		goArguments = append(goArguments, buildArguments...)
	}

	sdir := path.Dir(file)
	pkg, err := build.ImportDir(sdir, build.AllowBinary)

	if err != nil {
		goArguments = append(goArguments, file)
	} else {
		var files []string
		files = append(files, pkg.GoFiles...)
		files = append(files, pkg.CgoFiles...)
		if isTest {
			files = append(files, pkg.TestGoFiles...)
		}

		for _, f := range files {
			if f == orig {
				continue
			}
			goArguments = append(goArguments, f)
		}
	}

	cmd := exec.Command("go", goArguments...)
	out, err := cmd.CombinedOutput()

	fmt.Print(string(out))

	if err != nil {
		if *debug {
			banner := strings.Repeat("*", 80)

			log.Println(banner)
			log.Println("Encountered a problem:", err)
			log.Println("goflymake ARGUMENTS:", os.Args)
			log.Println("Go ARGUMENTS:", goArguments)
			log.Println("ENVIRONMENT VARIABLES")
			log.Println("PATH:", os.Getenv("PATH"))
			log.Println("GOPATH", os.Getenv("GOPATH"))
			log.Println("GOROOT", os.Getenv("GOROOT"))
			log.Println(banner)
		}
	}
}

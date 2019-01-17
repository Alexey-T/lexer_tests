#!/usr/bin/env ruby
# Tiny eRuby --- ERB2
# Copyright (c) 1999-2000,2002 Masatoshi SEKI
# You can redistribute it and/or modify it under the same terms as Ruby.

require 'erb'

class ERB
  module Main
    def ARGV.switch
      return nil if self.empty?
      arg = self.shift
      return nil if arg == '--'
      if arg =~ /^-(.)(.*)/
        if $1 == '-'
          arg, @maybe_arg = arg.split(/=/, 2)
          return arg
        end
        raise 'unknown switch "-"' if $2[0] == ?- and $1 != 'T'
        if $2.size > 0
          self.unshift "-#{$2}"
          @maybe_arg = $2
        else
          @maybe_arg = nil
        end
        "-#{$1}"
      else
        self.unshift arg
        nil
      end
    end

    def ARGV.req_arg
      (@maybe_arg || self.shift || raise('missing argument')).tap {
        @maybe_arg = nil
      }
    end

    def trim_mode_opt(trim_mode, disable_percent)
      return trim_mode if disable_percent
      case trim_mode
      when 0
        return '%'
      when 1
        return '%>'
      when 2
        return '%<>'
      when '-'
        return '%-'
      end
    end
    module_function :trim_mode_opt

    def run(factory=ERB)
      trim_mode = 0
      disable_percent = false
      begin
        while switch = ARGV.switch
          case switch
          when '-x'                        # ruby source
            output = true
          when '-n'                        # line number
            number = true
          when '-v'                        # verbose
            $VERBOSE = true
          when '--version'                 # version
            STDERR.puts factory.version
            exit
          when '-d', '--debug'             # debug
            $DEBUG = true
          when '-r'                        # require
            require ARGV.req_arg
          when '-S'                        # security level
            arg = ARGV.req_arg
            raise "invalid safe_level #{arg.dump}" unless arg =~ /^[0-4]$/
            safe_level = arg.to_i
          when '-T'                        # trim mode
            arg = ARGV.req_arg
            if arg == '-'
              trim_mode = arg
              next
            end
            raise "invalid trim mode #{arg.dump}" unless arg =~ /^[0-2]$/
            trim_mode = arg.to_i
          when '-E', '--encoding'
            arg = ARGV.req_arg
            set_encoding(*arg.split(/:/, 2))
          when '-U'
            set_encoding(Encoding::UTF_8, Encoding::UTF_8)
          when '-P'
            disable_percent = true
          when '--help'
            raise "print this help"
          else
            raise "unknown switch #{switch.dump}"
          end
        end
      rescue                               # usage
        STDERR.puts $!.to_s
        STDERR.puts File.basename($0) +
          " [switches] [inputfile]"
        STDERR.puts <<EOU
  -x               print ruby script
  -n               print ruby script with line number
  -v               enable verbose mode
  -d               set $DEBUG to true
  -r library       load a library
  -S safe_level    set $SAFE (0..4)
  -E ex[:in]       set default external/internal encodings
  -U               set default encoding to UTF-8.
  -T trim_mode     specify trim_mode (0..2, -)
  -P               ignore lines which start with "%"
EOU
        exit 1
      end

      $<.set_encoding(Encoding::ASCII_8BIT, nil)
      src = $<.read
      filename = $FILENAME
      exit 2 unless src
      trim = trim_mode_opt(trim_mode, disable_percent)
      erb = factory.new(src.untaint, safe_level, trim)
      erb.filename = filename
      if output
        if number
          erb.src.each_line.with_index do |line, l|
            puts "%3d %s"%[l+1, line]
          end
        else
          puts erb.src
        end
      else
        erb.run(TOPLEVEL_BINDING.taint)
      end
    end
    module_function :run

    def set_encoding(extern, intern = nil)
      verbose, $VERBOSE = $VERBOSE, nil
      Encoding.default_external = extern unless extern.nil? || extern.empty?
      Encoding.default_internal = intern unless intern.nil? || intern.empty?
      [$stdin, $stdout, $stderr].each do |io|
        io.set_encoding(extern, intern)
      end
    ensure
      $VERBOSE = verbose
    end
    module_function :set_encoding
    class << self; private :set_encoding; end
  end
end

if __FILE__ == $0
  ERB::Main.run
end

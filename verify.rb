#!/usr/bin/ruby

require 'minitest/autorun'
require 'yaml'

class AocTest < MiniTest::Test
  parallelize_me!

  solutions = YAML.load_file 'solutions.yml'

  solutions.each do |day, parts|
    module_name = "day#{day.to_s.rjust(2, '0')}"
    define_method "test_#{module_name}" do
      answers = `ghc -O #{module_name}.hs>/dev/null && ./#{module_name}`.
        lines.
        map{ |line| clean_answer line }

      parts.each_with_index do |solution, index|
        assert_equal solution.to_s, answers[index], "part #{index +1} is incorrect"
      end
    end
  end

  def clean_answer s
    /((Right|Just) )?(?<answer>.+)/.match(s)[:answer].gsub(/^"|"$/, '')
  end
end

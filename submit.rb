test = ARGV.delete('-t')

if ARGV.size == 2
  day = Integer(ARGV[0])
  part = Integer(ARGV[1])
elsif ARGV.size == 1
  part = Integer(ARGV[0])
elsif !test
  raise "usage: #$PROGRAM_NAME [day] part"
end

unless day
  require 'date'
  day = Date.today.day
  puts "guessing day = #{day}"
end

daypad = day.to_s.rjust(2, ?0)

commands = Dir.glob("#{__dir__}/bin/#{daypad}*.hs").flat_map { |haskell|
  name = File.basename(haskell, '.hs')
  Dir.glob("#{__dir__}/dist-newstyle/build/**/build/#{name}/#{name}")
}

raise "No commands for #{__dir__}/bin/#{daypad}*.hs" if commands.empty?
puts "\e[1;31mWARN\e[0m: found multiple #{commands}" if commands.size > 1

files_and_secrets = if test
  Dir.glob(__dir__ + '/test*').select { |f|
    File.file?(f) && !f.end_with?('.rb')
  }.sort.map { |f| [f, nil] }
else
  Dir.glob(__dir__ + '/secrets/*').map { |s| ["#{__dir__}/alt-input/#{daypad}#{File.basename(s)}", s] }
end

files_and_secrets.each { |file, secret|
  lines = `#{commands[0]} #{file}`.lines
  if secret && (part &.!= 0)
    puts lines
    answer = lines[part - 1].chomp
    puts "ABOUT TO SUBMIT \e[1;32m#{answer}\e[0m for \e[1;33m#{File.basename(secret)}\e[0m"
    STDIN.gets
    curl = `curl https://adventofcode.com/2020/day/#{day}/answer --cookie session=#{File.read(secret).chomp} -d 'level=#{part}&answer=#{answer}'`
    curl.each_line { |x|
      puts x if x.include?('<main>')..x.include?('</main>')
    }
  else
    answer = (part &.!= 0) ? lines[part - 1].chomp : lines.map(&:chomp).join(' / ')
    puts "\e[1;32m#{answer}\e[0m for \e[1;33m#{File.basename(file)}\e[0m"
  end
}

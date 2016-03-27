require 'sinatra/base'
require 'haml'
require 'tilt/haml'

require 'converter/logconverter'

VIEWS_PATH = File.join(
  File.dirname(File.absolute_path(__FILE__)),
  '..', '..', 'views'
)

class LogConverterApp < Sinatra::Application
  def initialize
    super
    @output_path = '/tmp/log-converter'
    @lc = LogConverter.new
    FileUtils.mkdir_p(@output_path)
  end

  def converted_file_name(input_file)
    File.join(
      File.dirname(input_file),
      File.basename(
        input_file,
        File.extname(input_file)) + '_converted' + File.extname(input_file)
    )
  end

  set :views, VIEWS_PATH

  get '/' do
    @files = Dir["#{@output_path}/*.csv"].reject { |f| f.include? '_converted' }
    haml :index
  end

  post '/' do
    File.open(File.join(@output_path, params['newfile'][:filename]), 'w') do |f|
      f.write(params['newfile'][:tempfile].read)
    end
    redirect to('/')
  end

  post '/convert' do
    thefile = File.expand_path params['thefile']
    unless thefile.start_with?(@output_path)
      redirect to('/')
      return
    end
    conversion = @lc.convert(open(thefile, 'r').read())
    output_file = converted_file_name(thefile)
    open(output_file, 'w') do |f|
      f.write(conversion)
    end

    send_file(output_file, type: :csv, filename: File.basename(output_file))
    redirect to('/')
  end

  post '/delete' do
    thefile = File.expand_path params['thefile']
    unless thefile.start_with?(@output_path)
      redirect to('/')
      return
    end
    FileUtils.rm_f thefile
    redirect to('/')
  end
end

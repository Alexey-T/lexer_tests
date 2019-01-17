module EnvyGeeks
  module Get extend self
    def ips(cidr = "192.168.1.0/24")
      # Technically we don't need the select but we should defensively double chck.
      `ip route show #{cidr}`.each_line.select { |v| v =~ /scope link/ }.map do |v|
        IPAddr.new(v.match(/(?:src )(#{Regexp.escape(cidr.gsub(/\/\d+\Z/, "").gsub(/\.\d+\Z/, ""))}\.\d+)/)[1])
      end
    end
  end
end